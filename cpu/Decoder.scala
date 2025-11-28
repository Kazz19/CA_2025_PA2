package cpu

import chisel3._
import chisel3.util._
import cpu.OP_TYPES._ 
import cpu.OPCODES_TYPES._ 

class Decoder_io extends Bundle {
    // Input command
    val command     = Input(UInt(32.W))
    
    // Register file
    val reg_addr    = Output(new RegAddr())     
    
    // Control signals
    val ctrl        = Output(new CtrlSignal())

    // Imm
    val imm         = Output(SInt(32.W))   
}

class Decoder extends Module {
    val io = IO(new Decoder_io())

    // Wires Declaration
    val funct7_w            = WireDefault(0.U(7.W))
    val funct3_w            = WireDefault(0.U(3.W))

    val rs1_addr_w          = WireDefault(0.U(5.W))
    val rs2_addr_w          = WireDefault(0.U(5.W))
    val rd_addr_w           = WireDefault(0.U(5.W))

    val imm_w               = WireDefault(0.S(32.W))

    val ctrlJump_w          = WireDefault(false.B)
    val ctrlJump_r_w        = WireDefault(false.B)
    val ctrlBranch_w        = WireDefault(false.B)
    val ctrlRegWrite_w      = WireDefault(true.B)
    val ctrlMemRead_w       = WireDefault(false.B)
    val ctrlMemWrite_w      = WireDefault(false.B)
    val ctrlALUSrc_w        = WireDefault(false.B)
    val ctrlALUOp_w         = WireDefault(0.U(5.W))
    val ctrlMemToReg_w      = WireDefault(false.B)

    val instr = io.command  
    val opcode_w = instr(6,0)
    
    // Temporary wire

    switch(opcode_w){
        is(OPCODE_R){                           // Arithmetic Instructions (R)
            funct3_w   := instr(14,12)
            funct7_w   := instr(31,25)
            
            rs1_addr_w := instr(19,15)
            rs2_addr_w := instr(24,20)
            rd_addr_w  := instr(11,7)
            
            switch(funct3_w){
                is("b000".U){
                    when(funct7_w === "b0000000".U){  // ADD case
                        ctrlALUOp_w    := OP_ADD
                    }.otherwise{                   // SUB case
                        ctrlALUOp_w := OP_SUB
                    }   
                }
                is("b001".U){  // SLL case
                    ctrlALUOp_w := OP_SLL}

                is("b010".U){  // SLT case
                    ctrlALUOp_w := OP_SLT}
                is("b100".U){  // XOR case
                    ctrlALUOp_w := OP_XOR}
                is("b101".U){  
                    when(funct7_w === "b0000000".U){  // SRL case
                        ctrlALUOp_w := OP_SRL
                    }.otherwise{                   // SRA case
                        ctrlALUOp_w := OP_SRA
                    }
                }
                is("b110".U){  // OR case 
                        ctrlALUOp_w := OP_OR}
                is("b111".U){  // AND case
                        ctrlALUOp_w := OP_AND}
            }
        }
        is(OPCODE_I){
            rs1_addr_w  := instr(19,15)
            rd_addr_w   := instr(11,7)
            imm_w       := instr(31,20).asSInt()
            funct3_w   := instr(14,12)
            switch(funct3_w){
                is("b000".U){      // ADDI
                    ctrlALUOp_w := OP_ADD}
                is("b001".U){      // SLLI
                    ctrlALUOp_w := OP_SLL}
                is("b010".U){      // SLTI
                    ctrlALUOp_w := OP_SLT}
                is("b101".U){
                    when(funct7_w === "b0000000".U){  // SRLI case
                        ctrlALUOp_w := OP_SRL
                    }.otherwise{                   // SRAI case
                        ctrlALUOp_w := OP_SRA
                    }
                }
            }     
            ctrlALUSrc_w    := true.B
        }
        is(OPCODE_LW){
            rs1_addr_w      := instr(19,15)
            rd_addr_w       := instr(11,7)
            ctrlALUOp_w     := OP_ADD
            ctrlALUSrc_w    := true.B
            ctrlMemRead_w   := true.B
            ctrlMemToReg_w  := true.B
            imm_w           := instr(31,20).asSInt
        }
        
        is(OPCODE_JALR){
            rs1_addr_w      := instr(19,15)
            rd_addr_w       := instr(11,7)
            imm_w           := instr(31,20).asSInt()
            ctrlALUOp_w     := OP_JALR
            ctrlALUSrc_w    := true.B
            ctrlJump_r_w    := true.B
        }

        is(OPCODE_LUI){
            rd_addr_w       := instr(11,7)
            imm_w           := instr(31,12).asSInt()
            ctrlALUOp_w     := OP_LUI
            ctrlALUSrc_w    := true.B
        }

        is(OPCODE_AUIPC){
            rd_addr_w       := instr(11,7)
            imm_w           := instr(31,12).asSInt()
            ctrlALUOp_w     := OP_AUIPC
            ctrlALUSrc_w    := true.B
        }
        is(OPCODE_S){
            rs1_addr_w      := instr(19,15)
            rs2_addr_w      := instr(24,20)
            imm_w           := Cat(instr(31,25),instr(11,7)).asSInt()
            ctrlALUOp_w     := OP_ADD
            ctrlRegWrite_w  := false.B
            ctrlALUSrc_w    := true.B
            ctrlMemWrite_w  := true.B
        }
        is(OPCODE_B){
            rs1_addr_w      := instr(19,15)
            rs2_addr_w      := instr(24,20)
            funct3_w        := instr(14,12)
            
            switch(funct3_w){
                is("b000".U){ctrlALUOp_w := OP_BEQ}
                is("b001".U){ctrlALUOp_w := OP_BNE}
                is("b100".U){ctrlALUOp_w := OP_BLT}
                is("b101".U){ctrlALUOp_w := OP_BGE}
            }
            imm_w           := Cat(instr(31),instr(7),instr(30,25),instr(11,8),0.U(1.W)).asSInt()
            ctrlBranch_w    := true.B
        }

        is(OPCODE_JAL){
            rd_addr_w       := instr(11,7)
            imm_w           := Cat(instr(31),instr(19,12),instr(20),instr(30,21),0.U(1.W)).asSInt()
            ctrlALUOp_w     := OP_JAL
            ctrlALUSrc_w    := true.B
            ctrlJump_w      := true.B
        }
    }


    // Outputs ctrl
    io.ctrl.ctrlJump        := ctrlJump_w 
    io.ctrl.ctrlJump_r      := ctrlJump_r_w           
    io.ctrl.ctrlBranch      := ctrlBranch_w   
    io.ctrl.ctrlRegWrite    := ctrlRegWrite_w   
    io.ctrl.ctrlMemRead     := ctrlMemRead_w   
    io.ctrl.ctrlMemWrite    := ctrlMemWrite_w   
    io.ctrl.ctrlALUSrc      := ctrlALUSrc_w   
    io.ctrl.ctrlALUOp       := ctrlALUOp_w   
    io.ctrl.ctrlMemToReg    := ctrlMemToReg_w   

    // Outputs reg
    io.reg_addr.rs1_addr    := rs1_addr_w 
    io.reg_addr.rs2_addr    := rs2_addr_w 
    io.reg_addr.rd_addr     := rd_addr_w

    // Output immgen
    io.imm          := imm_w.asSInt()
}   
