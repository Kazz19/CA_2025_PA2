package cpu

import chisel3._
import chisel3.util._
import cpu.OP_TYPES._

class Alu_io extends Bundle {
    val aluOp       = Input(UInt(5.W))
    val data_1      = Input(SInt(32.W))
    val data_2      = Input(SInt(32.W))
    val imm         = Input(SInt(32.W))
    val pc_counter  = Input(UInt(32.W))
    val ctrlALUSrc  = Input(Bool())
    val to_branch   = Output(Bool())
    val alu_result  = Output(SInt(32.W))
    val jump_addr   = Output(UInt(32.W))
    /// TODO ///
}

class Alu extends Module {
    val io = IO(new Alu_io())

    val to_branch  = WireDefault(false.B)
    val alu_result = WireDefault(0.S(32.W))
    val jump_addr  = WireDefault(0.U(32.W))
    
    val data_2_w   = Mux(io.ctrlALUSrc,io.imm,io.data_2)
    val data_2_U_w = data_2_w.asUInt()

    val result_compare = WireDefault(0.U(1.W))

    switch(io.aluOp){
        is(OP_NOP)  {alu_result := 0.S(32.W)}
        // ARITHMETIC OPERATIONS
        is(OP_ADD)  {alu_result := io.data_1 + data_2_w} 
        is(OP_SUB)  {alu_result := io.data_1 - data_2_w}   
        is(OP_AND)  {alu_result := io.data_1 & data_2_w}  
        is(OP_OR)   {alu_result := io.data_1 | data_2_w}   
        is(OP_XOR)  {alu_result := io.data_1 ^ data_2_w}
        
        // COMPARE OPERATIONS
        is(OP_SLT)  {
            result_compare := (io.data_1 < data_2_w).asUInt()
            alu_result := Cat(Fill(31,0.U),result_compare).asSInt
        }        
        // SHIFT OPERATIONS
        is(OP_SLL)  {alu_result := io.data_1 << data_2_U_w(4,0).asUInt()}
        is(OP_SRL)  {alu_result := (io.data_1.asUInt() >> data_2_U_w(4,0).asUInt()).asSInt()}    
        is(OP_SRA)  {alu_result := io.data_1 >> data_2_U_w(4,0).asUInt()}   

        // BRANCH OPERATIONS
        is(OP_BEQ)   {to_branch  := io.data_1 ===  data_2_w}     
        is(OP_BNE)   {to_branch  := io.data_1 =/=  data_2_w}    
        is(OP_BLT)   {to_branch  := io.data_1 < data_2_w}     
        is(OP_BGE)   {to_branch  := io.data_1 >=  data_2_w}  

        // JUMP OPERATIONS   
        is(OP_JAL)    {alu_result := (io.pc_counter.asSInt() + 4.S)}
        is(OP_JALR)   {
            alu_result := (io.pc_counter.asSInt() + 4.S)
            jump_addr  := ((io.data_1 + io.imm) & (~1).S).asUInt()}
        
        // OTHER OPERATIONS
        is(OP_LUI)      {alu_result := data_2_w << 12}
        is(OP_AUIPC)    {alu_result := io.pc_counter.asSInt() + (io.imm << 12)}

    }
    
    io.alu_result := alu_result
    io.to_branch  := to_branch
    io.jump_addr  := jump_addr
}