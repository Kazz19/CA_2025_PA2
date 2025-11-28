package cpu

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import firrtl.options.TargetDirAnnotation

// io for testbench to access
class Core_io extends Bundle {
    // for debugging
    val pc           = Output(UInt(32.W))
    val inst         = Output(UInt(32.W))
    val wb_result    = Output(UInt(32.W))
    // for grading, DO NOT MODIFY
    val peek_write   = Output(Bool())
    val peek_addr    = Output(UInt(32.W))
    val peek_data    = Output(UInt(32.W))
}

class Core extends Module {
    val io = IO(new Core_io())

    // instantiate all the modules
    val pc_handle = Module(new PCHandle())
    val inst_mem = Module(new InstMem())
    val decoder = Module(new Decoder())
    val reg_file = Module(new Regfile())
    val alu = Module(new Alu())
    val data_mem = Module(new DataMem())

    // Wire declaration
    val MEM_mux  = WireDefault(0.S(32.W))
        
    // DATA MEMORY
    data_mem.io.ctrlMemRead     := decoder.io.ctrl.ctrlMemRead
    data_mem.io.ctrlMemWrite    := decoder.io.ctrl.ctrlMemWrite
    data_mem.io.addr            := alu.io.alu_result.asUInt()
    data_mem.io.data_in         := reg_file.io.rs2_data.asUInt()

    // ALU
    alu.io.aluOp       := decoder.io.ctrl.ctrlALUOp
    alu.io.data_1      := reg_file.io.rs1_data
    alu.io.data_2      := reg_file.io.rs2_data
    alu.io.pc_counter  := pc_handle.io.pc_counter

    alu.io.imm         := decoder.io.imm
    alu.io.ctrlALUSrc  := decoder.io.ctrl.ctrlALUSrc

    // PC COUNTER
    pc_handle.io.ctrl_jump      := decoder.io.ctrl.ctrlJump
    pc_handle.io.ctrl_jump_r    := decoder.io.ctrl.ctrlJump_r
    pc_handle.io.jump_addr      := alu.io.jump_addr
    pc_handle.io.ctrl_branch    := decoder.io.ctrl.ctrlBranch
    pc_handle.io.branch_taken   := alu.io.to_branch
    pc_handle.io.imm            := decoder.io.imm
    
    // INSTRUCTION MEMORY 
    inst_mem.io.addr       := pc_handle.io.pc_counter

    // DECODER
    decoder.io.command     := inst_mem.io.inst

    // REG FILE
    reg_file.io.rs1_addr        := decoder.io.reg_addr.rs1_addr
    reg_file.io.rs2_addr        := decoder.io.reg_addr.rs2_addr
    reg_file.io.reg_write_en    := decoder.io.ctrl.ctrlRegWrite
    reg_file.io.reg_write_addr  := decoder.io.reg_addr.rd_addr
    reg_file.io.reg_write_data  := MEM_mux.asSInt()

    MEM_mux := Mux(decoder.io.ctrl.ctrlMemToReg,data_mem.io.data_out.asSInt(),alu.io.alu_result)   // connect all the modules here 

    // core
    io.pc            :=    pc_handle.io.pc_counter
    io.inst          :=    inst_mem.io.inst
    io.wb_result     :=    decoder.io.ctrl.ctrlRegWrite

    /// DO NOT MODIFY ///
    io.peek_write := data_mem.io.peek_write
    io.peek_addr := data_mem.io.addr
    io.peek_data := data_mem.io.data_in

}

/// You can add the following code
//  and generate verilog by command: sbt "runMain cpu.main" ///

// object main extends App {

//     (new ChiselStage).execute(
//     Array("--target-dir", "verilog_output"),
//     Seq(ChiselGeneratorAnnotation(() => new Core()))
//     )
// }


