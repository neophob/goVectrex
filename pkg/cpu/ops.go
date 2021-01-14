package cpu

import "../logger"

func (cpu *CPU) GetCycles(opcode uint8) uint64 {
	return []uint64{
		6, 0, 0, 6, 6, 0, 6, 6, 6, 6, 6, 0, 6, 6, 3, 6, /* 00-0F */
		1, 1, 2, 2, 0, 0, 5, 9, 0, 2, 3, 0, 3, 2, 8, 7, /* 10-1F */
		3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /* 20-2F */
		4, 4, 4, 4, 5, 5, 5, 5, 0, 5, 3, 6, 21, 11, 0, 19, /* 30-3F */
		2, 0, 0, 2, 2, 0, 2, 2, 2, 2, 2, 0, 2, 2, 0, 2, /* 40-4F */
		2, 0, 0, 2, 2, 0, 2, 2, 2, 2, 2, 0, 2, 2, 0, 2, /* 50-5F */
		6, 0, 0, 6, 6, 0, 6, 6, 6, 6, 6, 0, 6, 6, 3, 6, /* 60-6F */
		7, 0, 0, 7, 7, 0, 7, 7, 7, 7, 7, 0, 7, 7, 4, 7, /* 70-7F */
		2, 2, 2, 4, 2, 2, 2, 0, 2, 2, 2, 2, 4, 7, 3, 0, /* 80-8F */
		4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 6, 7, 5, 5, /* 90-9F */
		4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 6, 7, 5, 5, /* A0-AF */
		5, 5, 5, 7, 5, 5, 5, 5, 5, 5, 5, 5, 7, 8, 6, 6, /* B0-BF */
		2, 2, 2, 4, 2, 2, 2, 0, 2, 2, 2, 2, 3, 0, 3, 0, /* C0-CF */
		4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, /* D0-DF */
		4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, /* E0-EF */
		5, 5, 5, 7, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, /* F0-FF */
	}[opcode]
}

// GetFlagsNZ returns Negative and zero flags for quicker flag settings
func (cpu *CPU) GetFlagsNZ(opcode uint8) uint8 {
	return []uint8{
		4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 00-0F */
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 10-1F */
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 20-2F */
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 30-3F */
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 40-4F */
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 50-5F */
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 60-6F */
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 70-7F */
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, /* 80-8F */
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, /* 90-9F */
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, /* A0-AF */
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, /* B0-BF */
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, /* C0-CF */
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, /* D0-DF */
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, /* E0-EF */
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, /* F0-FF */
	}[opcode]
}

func (cpu *CPU) getD() uint16 {
	return uint16(cpu.regA)<<8 + uint16(cpu.regB)
}

func (cpu *CPU) setD(v uint16) {
	cpu.regA = uint8(v >> 8)
	cpu.regB = uint8(v & 0xff)
}

func signed(x uint8) int8 {
	if x > 0x7F {
		return x - 0x100
	}
	return x
}

func signed16(x uint16) int16 {
	if x > 0x7fff {
		return x - 0x10000
	}
	return x
}

func (cpu *CPU) PUSHB(data uint8) {
	cpu.regS = (cpu.regS - 1) & 0xFFFF
	cpu.write(cpu.regS, data)
}

func (cpu *CPU) PUSHW(data uint16) {
	cpu.regS = (cpu.regS - 1) & 0xFFFF
	cpu.write(cpu.regS, uint8(data&0xFF))

	cpu.regS = (cpu.regS - 1) & 0xFFFF
	cpu.write(cpu.regS, uint8((data>>8)&0xFF))
}

func (cpu *CPU) dpadd() uint16 {
	//direct page + 8bit index
	return uint16(cpu.regDP)<<8 + uint16(cpu.fetch())
}

func (cpu *CPU) flagsNZ16(word uint16) {
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagZero | flagNegative)
	if word == 0 {
		cpu.regCC |= flagZero
	}
	if (word & 0x8000) > 0 {
		cpu.regCC |= flagNegative
	}
}

func (cpu *CPU) RunOpCode() uint64 {
	opcode := cpu.fetch()
	cpu.tickCount += cpu.GetCycles(opcode)

	switch opcode {
	case 0x00: //NEG DP
		addr := cpu.dpadd()
		cpu.write(
			addr,
			cpu.oNEG(cpu.read(addr)))

	case 0x03: //COM DP
		addr := cpu.dpadd()
		cpu.write(
			addr,
			cpu.oCOM(cpu.read(addr)))

	case 0x04: //LSR DP
		addr := cpu.dpadd()
		cpu.write(
			addr,
			cpu.oLSR(cpu.read(addr)))

	case 0x06: //ROR DP
		addr := cpu.dpadd()
		cpu.write(
			addr,
			cpu.oROR(cpu.read(addr)))

	case 0x07: //ASR DP
		addr := cpu.dpadd()
		cpu.write(
			addr,
			cpu.oASR(cpu.read(addr)))

	case 0x08: //ASL DP
		addr := cpu.dpadd()
		cpu.write(
			addr,
			cpu.oASL(cpu.read(addr)))

	case 0x09: //ROL DP
		addr := cpu.dpadd()
		cpu.write(
			addr,
			cpu.oROL(cpu.read(addr)))

	case 0x0a: //DEC DP
		addr := cpu.dpadd()
		cpu.write(
			addr,
			cpu.oDEC(cpu.read(addr)))

	case 0x0c: //INC DP
		addr := cpu.dpadd()
		cpu.write(
			addr,
			cpu.oINC(cpu.read(addr)))

	case 0x0d: //TST DP
		addr := cpu.dpadd()
		pb := cpu.read(addr)
		//TODO was tilde, unclear
		cpu.regCC &= ^(flagZero | flagNegative | flagOverflow)
		cpu.regCC |= cpu.GetFlagsNZ(pb)

	case 0x0e: //JMP DP
		addr := cpu.dpadd()
		cpu.regPC = addr

	case 0x0f: //CLR DP
		addr := cpu.dpadd()
		cpu.write(addr, 0)
		//TODO was tilde, unclear
		cpu.regCC &= ^(flagCarry | flagNegative | flagOverflow)
		cpu.regCC |= flagZero

	case 0x12: //NOP
		break

	case 0x13: //SYNC
		/*
			This commands stops the CPU, brings the processor bus to high impedance state and waits for an interrupt.
		*/
		logger.Error("SYNC is broken!")

	case 0x16: //LBRA relative
		addr := cpu.fetch16()
		cpu.regPC += addr

	case 0x17: //LBSR relative
		addr := cpu.fetch16()
		cpu.PUSHW(cpu.regPC)
		cpu.regPC += addr

	case 0x19:
		{ //DAA
			correctionFactor := uint16(0)
			nhi := cpu.regA & 0xF0
			nlo := cpu.regA & 0x0F
			if nlo > 0x09 || (cpu.regCC&flagHalfCarry) > 0 {
				correctionFactor |= 0x06
			}
			if nhi > 0x80 && nlo > 0x09 {
				correctionFactor |= 0x60
			}
			if nhi > 0x90 || (cpu.regCC&flagCarry) > 0 {
				correctionFactor |= 0x60
			}
			addr := correctionFactor + uint16(cpu.regA)
			// TODO Check, mame does not clear carry here
			//TODO was tilde, unclear
			cpu.regCC &= ^(flagCarry | flagNegative | flagZero | flagOverflow)
			if (addr & 0x100) > 0 {
				cpu.regCC |= flagCarry
			}
			cpu.regA = uint8(addr & 0xFF)
			cpu.regCC |= cpu.GetFlagsNZ(cpu.regA)
			break
		}
	case 0x1a: //ORCC
		cpu.regCC |= cpu.fetch()

	case 0x1c: //ANDCC
		cpu.regCC &= cpu.fetch()

	case 0x1d: //SEX
		//TODO should we use signed here?
		if (cpu.regB & 0x80) > 0 {
			cpu.regA = 0xFF
		} else {
			cpu.regA = 0
		}
		cpu.flagsNZ16(cpu.getD())

	case 0x1e: //EXG
		pb := cpu.fetch()
		cpu.TFREXG(pb, true)

	case 0x1f: //TFR
		pb := cpu.fetch()
		cpu.TFREXG(pb, false)

	case 0x20: //BRA
		addr := cpu.signed(cpu.fetch())
		cpu.regPC += addr

	}
	return 0
}

func (cpu *CPU) oNEG(b uint8) uint8 {
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagCarry | flagZero | flagOverflow | flagNegative)
	b = (0 - b) & 0xFF
	if b == 0x80 {
		cpu.regCC |= flagOverflow
	}
	if b == 0 {
		cpu.regCC |= flagZero
	}
	if (b & 0x80) > 0 {
		cpu.regCC |= flagNegative | flagCarry
	}
	return b
}

func (cpu *CPU) oCOM(b uint8) uint8 {
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagZero | flagNegative | flagOverflow)
	b ^= 0xFF
	b &= 0xFF
	cpu.regCC |= cpu.GetFlagsNZ(b)
	cpu.regCC |= flagCarry
	return b
}

func (cpu *CPU) oLSR(b uint8) uint8 {
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagZero | flagCarry | flagNegative)
	if (b & flagCarry) > 0 {
		cpu.regCC |= flagCarry
	}
	b >>= 1
	if b == 0 {
		cpu.regCC |= flagZero
	}
	return b
}

func (cpu *CPU) oROR(b uint8) uint8 {
	oldCarry := cpu.regCC & flagCarry
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagZero | flagCarry | flagNegative)
	if (b & 0x01) > 0 {
		cpu.regCC |= flagCarry
	}
	b = (b >> 1) | (oldCarry << 7)
	b &= 0xFF
	cpu.regCC |= cpu.GetFlagsNZ(b)
	return b
}

func (cpu *CPU) oASR(b uint8) uint8 {
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagZero | flagCarry | flagNegative)
	if (b & 0x01) > 0 {
		cpu.regCC |= flagCarry
	}
	b = (b & 0x80) | (b >> 1)
	b &= 0xFF
	cpu.regCC |= cpu.GetFlagsNZ(b)
	return b
}

func (cpu *CPU) oASL(b uint8) uint8 {
	temp := b
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagZero | flagCarry | flagNegative | flagOverflow)
	if (b & 0x80) > 0 {
		cpu.regCC |= flagCarry
	}
	b <<= 1
	if ((b ^ temp) & 0x80) > 0 {
		cpu.regCC |= flagOverflow
	}
	b &= 0xFF
	cpu.regCC |= cpu.GetFlagsNZ(b)
	return b
}

func (cpu *CPU) oROL(b uint8) uint8 {
	temp := b
	oldCarry := cpu.regCC & flagCarry
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagZero | flagCarry | flagNegative | flagOverflow)
	if (b & 0x80) > 0 {
		cpu.regCC |= flagCarry
	}
	b = (b << 1) | oldCarry
	if ((b ^ temp) & 0x80) > 0 {
		cpu.regCC |= flagOverflow
	}
	b &= 0xFF
	cpu.regCC |= cpu.GetFlagsNZ(b)
	return b
}

func (cpu *CPU) oDEC(b uint8) uint8 {
	b = (b - 1) & 0xFF
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagZero | flagOverflow | flagNegative)
	cpu.regCC |= cpu.GetFlagsNZ(b)
	//Docs say:
	//V: Set if the original operand was 10000000
	if b == 0x7f {
		cpu.regCC |= flagOverflow
	}
	return b
}

func (cpu *CPU) oINC(b uint8) uint8 {
	b = (b + 1) & 0xFF
	//TODO was tilde, unclear
	cpu.regCC &= ^(flagZero | flagOverflow | flagNegative)
	cpu.regCC |= cpu.GetFlagsNZ(b)
	//Docs say:
	//V: Set if the original operand was 01111111
	if b == 0x80 {
		cpu.regCC |= flagOverflow
	}
	return b
}

// Transfer or exchange two registers.
func (cpu *CPU) TFREXG(ucPostByte uint8, bExchange bool) {
	ucTemp := uint16(ucPostByte & 0x88)
	if ucTemp == 0x80 || ucTemp == 0x08 {
		logger.Error("TFREXG_ERROR_MIXING_8_AND_16BIT_REGISTER!")
	}

	ucTemp = cpu.getPostByteRegister(ucPostByte >> 4)
	if bExchange {
		cpu.setPostByteRegister(ucPostByte>>4, cpu.getPostByteRegister(ucPostByte))
	}
	/* Transfer */
	cpu.setPostByteRegister(ucPostByte, ucTemp)
}

func (cpu *CPU) getPostByteRegister(ucPostByte uint8) uint16 {
	switch ucPostByte & 0xF {
	case 0x00:
		return cpu.getD()
	case 0x1:
		return cpu.regX
	case 0x2:
		return cpu.regY
	case 0x3:
		return cpu.regU
	case 0x4:
		return cpu.regS
	case 0x5:
		return cpu.regPC
	case 0x8:
		return uint16(cpu.regA)
	case 0x9:
		return uint16(cpu.regB)
	case 0xA:
		return uint16(cpu.regCC)
	case 0xB:
		return uint16(cpu.regDP)
	default:
		/* illegal */
		logger.Error("getPBR_INVALID_%d", ucPostByte)
		return 0
	}
}

func (cpu *CPU) setPostByteRegister(ucPostByte uint8, v uint16) {
	/* Get destination register */
	switch ucPostByte & 0xF {
	case 0x00:
		cpu.setD(v)
		return
	case 0x1:
		cpu.regX = v
		return
	case 0x2:
		cpu.regY = v
		return
	case 0x3:
		cpu.regU = v
		return
	case 0x4:
		cpu.regS = v
		return
	case 0x5:
		cpu.regPC = v
		return
	case 0x8:
		cpu.regA = uint8(v)
		return
	case 0x9:
		cpu.regB = uint8(v)
		return
	case 0xA:
		cpu.regCC = uint8(v)
		return
	case 0xB:
		cpu.regDP = uint8(v)
		return
	default:
		/* illegal */
		logger.Error("setPBR_INVALID_%d", ucPostByte)
	}
}
