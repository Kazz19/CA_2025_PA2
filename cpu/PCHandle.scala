package cpu

import chisel3._
import chisel3.util._

class PCHandle_io extends Bundle {
    val ctrl_jump_r     = Input(Bool())
    val ctrl_jump       = Input(Bool())
    val ctrl_branch     = Input(Bool())
    val branch_taken    = Input(Bool())
    val imm             = Input(SInt(32.W))
    val jump_addr       = Input(UInt(32.W))
    val pc_counter      = Output(UInt(32.W))   
}

class PCHandle extends Module {
    val io = IO(new PCHandle_io())  
    
    val pc = RegInit(UInt(32.W), 0.U) 
    
    val pcPlus4         = pc + 4.U
    val branchTarget    = (pc.asSInt() + io.imm).asUInt()   
    val jumpTarget      = (pc.asSInt() + io.imm).asUInt()

    
    when(io.ctrl_branch && io.branch_taken){
        pc := branchTarget
    }.elsewhen(io.ctrl_jump_r){
        pc  := io.jump_addr
    }.elsewhen(io.ctrl_jump){
        pc  := jumpTarget
    }.otherwise{
        pc  := pcPlus4 
    }
    io.pc_counter := pc
}