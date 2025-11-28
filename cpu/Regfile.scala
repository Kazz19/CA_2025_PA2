package cpu

import chisel3._
import chisel3.util._

class Regfile_io extends Bundle {
    val rs1_addr        = Input(UInt(5.W))
    val rs2_addr        = Input(UInt(5.W))
    val reg_write_en    = Input(Bool())
    val reg_write_addr  = Input(UInt(5.W))
    val reg_write_data  = Input(SInt(32.W))
    
    val rs1_data        = Output(SInt(32.W))
    val rs2_data        = Output(SInt(32.W))
}

class Regfile extends Module {
    val io = IO(new Regfile_io())

    // declare 32 registers each 32 bits
    val regs = Reg(Vec(32, SInt(32.W)))

    // r0 = zero
    regs(0)  := 0.S

    // Reading 
    io.rs1_data := regs(io.rs1_addr)
    io.rs2_data := regs(io.rs2_addr)

    // Writing 
    when(io.reg_write_en && io.reg_write_addr =/= 0.U){
        regs(io.reg_write_addr)  := io.reg_write_data
    }
}
