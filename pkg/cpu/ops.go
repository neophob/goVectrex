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

func (cpu *CPU) signed5bit(x uint8) int8 {
	if x > 0xF {
		return int8(x) - 0x20
	}
	return int8(x)
}

func (cpu *CPU) signed(x uint8) int8 {
	if x > 0x7F {
		return int8(uint16(x) - 0x100)
	}
	return int8(x)
}

func (cpu *CPU) signed16(x uint16) int16 {
	if x > 0x7fff {
		return int16(uint32(x) - 0x10000)
	}
	return int16(x)
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

func (cpu *CPU) PUSHBU(b uint8) {
	cpu.regU = (cpu.regU - 1) & 0xFFFF
	cpu.write(cpu.regU, b)
}

func (cpu *CPU) PUSHWU(b uint16) {
	cpu.regU = (cpu.regU - 1) & 0xFFFF
	cpu.write(cpu.regU, uint8(b&0xFF))
	cpu.regU = (cpu.regU - 1) & 0xFFFF
	cpu.write(cpu.regU, uint8((b>>8)&0xFF))
}

func (cpu *CPU) PULLB() uint8 {
	cpu.regS++
	return cpu.read(cpu.regS)
}

func (cpu *CPU) PULLW() uint16 {
	cpu.regS++
	b1 := cpu.regS
	cpu.regS++
	b2 := cpu.regS
	return uint16(cpu.read(b1)<<8) + uint16(cpu.read(b2))
}

func (cpu *CPU) PULLBU() uint8 {
	cpu.regU++
	return cpu.read(cpu.regU)
}

func (cpu *CPU) PULLWU() uint16 {
	cpu.regU++
	u1 := cpu.regU
	cpu.regU++
	u2 := cpu.regU
	return uint16(cpu.read(u1)<<8) + uint16(cpu.read(u2))
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
		//TODO sure with the signess?
		cpu.regPC += uint16(addr)

	case 0x21: //BRN
		cpu.signed(cpu.fetch())
		//TODO WHUUT?? nothing is done?   PC += 1 might be another option

	case 0x22: //BHI
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & (flagCarry | flagZero)) == 0 {
			//TODO sure with the signess?
			cpu.regPC += uint16(addr)
		}

	case 0x23: //BLS
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & (flagCarry | flagZero)) > 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x24: //BCC
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & flagCarry) == 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x25: //BCS
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & flagCarry) > 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x26: //BNE
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & flagZero) == 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x27: //BEQ
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & flagZero) > 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x28: //BVC
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & flagOverflow) == 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x29: //BVS
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & flagOverflow) > 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x2a: //BPL
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & flagNegative) == 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x2b: //BMI
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC & flagNegative) > 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x2c: //BGE
		addr := cpu.signed(cpu.fetch())
		if ((cpu.regCC & flagNegative) ^ ((cpu.regCC & flagOverflow) << 2)) == 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x2d: //BLT
		addr := cpu.signed(cpu.fetch())
		if ((cpu.regCC & flagNegative) ^ ((cpu.regCC & flagOverflow) << 2)) > 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x2e: //BGT TODO VALIDATE
		addr := cpu.signed(cpu.fetch())
		if ((cpu.regCC & flagNegative) ^ ((cpu.regCC & flagOverflow) << 2) | (cpu.regCC & flagZero)) == 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x2f: //BLE TODO VALIDATE
		addr := cpu.signed(cpu.fetch())
		if (cpu.regCC&flagNegative)^((cpu.regCC&flagOverflow)<<2) > 0 || (cpu.regCC&flagZero) > 0 {
			cpu.regPC += uint16(addr)
		}

	case 0x30: //LEAX
		cpu.regX = cpu.PostByte()
		//TODO was tilde
		cpu.regCC &= ^flagZero
		if cpu.regX == 0 {
			cpu.regCC |= flagZero
		}

	case 0x31: //LEAY
		cpu.regY = cpu.PostByte()
		//TODO was tilde
		cpu.regCC &= ^flagZero
		if cpu.regY == 0 {
			cpu.regCC |= flagZero
		}

	case 0x32: //LEAS
		cpu.regS = cpu.PostByte()

	case 0x33: //LEAU
		cpu.regU = cpu.PostByte()

	case 0x34: //PSHS
		cpu.PSHS(cpu.fetch())

	case 0x35: //PULS
		cpu.PULS(cpu.fetch())

	case 0x36: //PSHU
		cpu.PSHU(cpu.fetch())

	case 0x37: //PULU
		cpu.PULU(cpu.fetch())

	case 0x39: //RTS
		cpu.regPC = cpu.PULLW()

	case 0x3a: //ABX
		cpu.regX += uint16(cpu.regB)

	case 0x3b: //RTI
		cpu.regCC = cpu.PULLB()
		logger.Debug("RTI %d, %d", cpu.regCC&flagEntire, cpu.tickCount)
		// Check for fast interrupt
		if (cpu.regCC & flagEntire) > 0 {
			cpu.tickCount += 9
			cpu.regA = cpu.PULLB()
			cpu.regB = cpu.PULLB()
			cpu.regDP = cpu.PULLB()
			cpu.regX = cpu.PULLW()
			cpu.regY = cpu.PULLW()
			cpu.regU = cpu.PULLW()
		}
		cpu.regPC = cpu.PULLW()

	case 0x3c: //CWAI
		logger.Warn("CWAI is broken!")
		/*
		 * CWAI stacks the entire machine state on the hardware stack,
		 * then waits for an interrupt; when the interrupt is taken
		 * later, the state is *not* saved again after CWAI.
		 * see mame-6809.c how to proper implement this opcode
		 */
		cpu.regCC &= cpu.fetch()
	//TODO - ??? set cwai flag to true, do not exec next interrupt (NMI, FIRQ, IRQ) - but set reset cwai flag afterwards

	case 0x3d: //MUL
		addr := uint16(cpu.regA) * uint16(cpu.regB)
		if addr == 0 {
			cpu.regCC |= flagZero
		} else {
			//TODO Was tilde
			cpu.regCC &= ^flagZero
		}
		if (addr & 0x80) > 0 {
			cpu.regCC |= flagCarry
		} else {
			//TODO Was tilde
			cpu.regCC &= ^flagCarry
		}
		cpu.setD(addr)
		break

	case 0x3f: //SWI
		logger.Warn("SWI is untested!")
		cpu.regCC |= flagEntire
		cpu.PUSHW(cpu.regPC)
		cpu.PUSHW(cpu.regU)
		cpu.PUSHW(cpu.regY)
		cpu.PUSHW(cpu.regX)
		cpu.PUSHB(cpu.regDP)
		cpu.PUSHB(cpu.regB)
		cpu.PUSHB(cpu.regA)
		cpu.PUSHB(cpu.regCC)
		cpu.regCC |= flagIrqMask | flagFirqMask
		cpu.regPC = cpu.readWord(vectorSwi)

	case 0x40:
		cpu.regA = cpu.oNEG(cpu.regA)

	case 0x43:
		cpu.regA = cpu.oCOM(cpu.regA)

	case 0x44:
		cpu.regA = cpu.oLSR(cpu.regA)

	case 0x46:
		cpu.regA = cpu.oROR(cpu.regA)

	case 0x47:
		cpu.regA = cpu.oASR(cpu.regA)

	case 0x48:
		cpu.regA = cpu.oASL(cpu.regA)

	case 0x49:
		cpu.regA = cpu.oROL(cpu.regA)

	case 0x4a:
		cpu.regA = cpu.oDEC(cpu.regA)

	case 0x4c:
		cpu.regA = cpu.oINC(cpu.regA)

	case 0x4d: // tsta
		//TODO was tilde
		cpu.regCC &= ^(flagZero | flagNegative | flagOverflow)
		cpu.regCC |= cpu.GetFlagsNZ(cpu.regA & 0xFF)

	case 0x4f: /* CLRA */
		cpu.regA = 0
		//TODO was tilde
		cpu.regCC &= ^(flagNegative | flagOverflow | flagCarry)
		cpu.regCC |= flagZero

	case 0x50: /* NEGB */
		cpu.regB = cpu.oNEG(cpu.regB)

	case 0x53:
		cpu.regB = cpu.oCOM(cpu.regB)

	case 0x54:
		cpu.regB = cpu.oLSR(cpu.regB)

	case 0x56:
		cpu.regB = cpu.oROR(cpu.regB)

	case 0x57:
		cpu.regB = cpu.oASR(cpu.regB)

	case 0x58:
		cpu.regB = cpu.oASL(cpu.regB)

	case 0x59:
		cpu.regB = cpu.oROL(cpu.regB)

	case 0x5a:
		cpu.regB = cpu.oDEC(cpu.regB)

	case 0x5c: // INCB
		cpu.regB = cpu.oINC(cpu.regB)

	case 0x5d: /* TSTB */
		cpu.regCC &= ^(flagZero | flagNegative | flagOverflow)
		cpu.regCC |= cpu.GetFlagsNZ(cpu.regB & 0xFF)

	case 0x5f: //CLRB
		cpu.regB = 0
		cpu.regCC &= ^(flagNegative | flagOverflow | flagCarry)
		cpu.regCC |= flagZero

	case 0x60: //NEG indexed
		addr := cpu.PostByte()
		cpu.write(addr, cpu.oNEG(cpu.read(addr)))

	case 0x63: //COM indexed
		addr := cpu.PostByte()
		cpu.write(addr, cpu.oCOM(cpu.read(addr)))

	case 0x64: //LSR indexed
		addr := cpu.PostByte()
		cpu.write(addr, cpu.oLSR(cpu.read(addr)))

	case 0x66: //ROR indexed
		addr := cpu.PostByte()
		cpu.write(addr, cpu.oROR(cpu.read(addr)))

	case 0x67: //ASR indexed
		addr := cpu.PostByte()
		cpu.write(addr, cpu.oASR(cpu.read(addr)))

	case 0x68: //ASL indexed
		addr := cpu.PostByte()
		cpu.write(addr, cpu.oASL(cpu.read(addr)))

	case 0x69: //ROL indexed
		addr := cpu.PostByte()
		cpu.write(addr, cpu.oROL(cpu.read(addr)))

	case 0x6a: //DEC indexed
		addr := cpu.PostByte()
		cpu.write(addr, cpu.oDEC(cpu.read(addr)))

	case 0x6c: //INC indexed
		addr := cpu.PostByte()
		cpu.write(addr, cpu.oINC(cpu.read(addr)))

	case 0x6d: //TST indexed
		addr := cpu.PostByte()
		pb := cpu.read(addr)
		cpu.regCC &= ^(flagZero | flagNegative | flagOverflow)
		cpu.regCC |= cpu.GetFlagsNZ(pb & 0xFF)

	case 0x6e: //JMP indexed
		addr := cpu.PostByte()
		cpu.regPC = addr

	case 0x6f: //CLR indexed
		addr := cpu.PostByte()
		cpu.write(addr, 0)
		cpu.regCC &= ^(flagNegative | flagOverflow | flagCarry)
		cpu.regCC |= flagZero

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

func (cpu *CPU) PostByte() uint16 {
	INDIRECT_FIELD := uint8(0x10)
	REGISTER_FIELD := uint8(0x60)
	COMPLEXTYPE_FIELD := uint8(0x80)
	ADDRESSINGMODE_FIELD := uint8(0x0F)

	postByte := cpu.fetch()
	registerField := uint16(0)
	// Isolate register is used for the indexed operation
	// see Table 3-6. Indexed Addressing Postbyte Register
	switch postByte & REGISTER_FIELD {
	case 0x00:
		registerField = cpu.regX
		break
	case 0x20:
		registerField = cpu.regY
		break
	case 0x40:
		registerField = cpu.regU
		break
	case 0x60:
		registerField = cpu.regS
		break
	default:
		logger.Error("INVALID_ADDRESS_PB: %d", postByte)
	}

	xchg := int32(-1)
	EA := uint16(0)
	if (postByte & COMPLEXTYPE_FIELD) > 0 {
		// Complex stuff
		switch postByte & ADDRESSINGMODE_FIELD {
		case 0x00: // R+
			EA = registerField
			xchg = int32(registerField) + 1
			cpu.tickCount += 2
			break
		case 0x01: // R++
			EA = registerField
			xchg = int32(registerField) + 2
			cpu.tickCount += 3
			break
		case 0x02: // -R
			xchg = int32(registerField) - 1
			EA = uint16(xchg)
			cpu.tickCount += 2
			break
		case 0x03: // --R
			xchg = int32(registerField) - 2
			EA = uint16(xchg)
			cpu.tickCount += 3
			break
		case 0x04: // EA = R + 0 OFFSET
			EA = registerField
			break
		case 0x05: // EA = R + REGB OFFSET
			EA = registerField + uint16(cpu.signed(cpu.regB))
			cpu.tickCount++
			break
		case 0x06: // EA = R + REGA OFFSET
			EA = registerField + uint16(cpu.signed(cpu.regA))
			cpu.tickCount++
			break
		// case 0x07 is ILLEGAL
		case 0x08: // EA = R + 7bit OFFSET
			EA = registerField + uint16(cpu.signed(cpu.fetch()))
			cpu.tickCount++
			break
		case 0x09: // EA = R + 15bit OFFSET
			EA = registerField + uint16(cpu.signed16(cpu.fetch16()))
			cpu.tickCount += 4
			break
		// case 0x0A is ILLEGAL
		case 0x0B: // EA = R + D OFFSET
			EA = registerField + cpu.getD()
			cpu.tickCount += 4
			break
		case 0x0C:
			{ // EA = PC + 7bit OFFSET
				// NOTE: fetch increases regPC - so order is important!
				byte := cpu.signed(cpu.fetch())
				EA = cpu.regPC + uint16(byte)
				cpu.tickCount++
				break
			}
		case 0x0D:
			{ // EA = PC + 15bit OFFSET
				// NOTE: fetch increases regPC - so order is important!
				word := cpu.signed16(cpu.fetch16())
				EA = cpu.regPC + uint16(word)
				cpu.tickCount += 5
				break
			}
		// case 0xE is ILLEGAL
		case 0x0F: // EA = ADDRESS
			EA = cpu.fetch16()
			cpu.tickCount += 5
			break
		default:
			{
				mode := postByte & ADDRESSINGMODE_FIELD
				logger.Error("INVALID_ADDRESS_MODE_%d", mode)
			}
		}

		EA &= 0xFFFF
		if (postByte & INDIRECT_FIELD) > 0 {
			/* TODO: Indirect "Increment/Decrement by 1" is not valid
			const adrmode = postByte & ADDRESSINGMODE_FIELD
			if (adrmode === 0 || adrmode === 2) {
				throw new Error('INVALID_INDIRECT_ADDRESSMODE_', adrmode);
			}
			*/
			// INDIRECT addressing
			EA = cpu.readWord(EA)
			cpu.tickCount += 3
		}
	} else {
		// Just a 5 bit signed offset + register, NO INDIRECT ADDRESS MODE
		sByte := cpu.signed5bit(postByte & 0x1F)
		EA = registerField + uint16(sByte)
		cpu.tickCount++
	}

	if xchg != -1 {
		xchg &= 0xFFFF
		switch postByte & REGISTER_FIELD {
		case 0:
			cpu.regX = uint16(xchg)
			break
		case 0x20:
			cpu.regY = uint16(xchg)
			break
		case 0x40:
			cpu.regU = uint16(xchg)
			break
		case 0x60:
			cpu.regS = uint16(xchg)
			break
		default:
			logger.Error("PB_INVALID_XCHG_VALUE_%d", postByte)
		}
	}
	// Return the effective address
	return EA & 0xFFFF
}

func (cpu *CPU) PSHS(ucTemp uint8) {
	i := 0
	if (ucTemp & 0x80) > 0 {
		cpu.PUSHW(cpu.regPC)
		i += 2
	}
	if (ucTemp & 0x40) > 0 {
		cpu.PUSHW(cpu.regU)
		i += 2
	}
	if (ucTemp & 0x20) > 0 {
		cpu.PUSHW(cpu.regY)
		i += 2
	}
	if (ucTemp & 0x10) > 0 {
		cpu.PUSHW(cpu.regX)
		i += 2
	}
	if (ucTemp & 0x8) > 0 {
		cpu.PUSHB(cpu.regDP)
		i++
	}
	if (ucTemp & 0x4) > 0 {
		cpu.PUSHB(cpu.regB)
		i++
	}
	if (ucTemp & 0x2) > 0 {
		cpu.PUSHB(cpu.regA)
		i++
	}
	if (ucTemp & 0x1) > 0 {
		cpu.PUSHB(cpu.regCC)
		i++
	}
	cpu.tickCount++
}

//Pull A, B, CC, DP, D, X, Y, U, or PC from hardware stack
func (cpu *CPU) PULS(ucTemp uint8) {
	i := uint64(0)
	if (ucTemp & 0x1) > 0 {
		cpu.regCC = cpu.PULLB()
		i++
	}
	if (ucTemp & 0x2) > 0 {
		cpu.regA = cpu.PULLB()
		i++
	}
	if (ucTemp & 0x4) > 0 {
		cpu.regB = cpu.PULLB()
		i++
	}
	if (ucTemp & 0x8) > 0 {
		cpu.regDP = cpu.PULLB()
		i++
	}
	if (ucTemp & 0x10) > 0 {
		cpu.regX = cpu.PULLW()
		i += 2
	}
	if (ucTemp & 0x20) > 0 {
		cpu.regY = cpu.PULLW()
		i += 2
	}
	if (ucTemp & 0x40) > 0 {
		cpu.regU = cpu.PULLW()
		i += 2
	}
	if (ucTemp & 0x80) > 0 {
		cpu.regPC = cpu.PULLW()
		i += 2
	}
	cpu.tickCount += i //timing
}

//Push A, B, CC, DP, D, X, Y, S, or PC onto user stack
func (cpu *CPU) PSHU(ucTemp uint8) {
	i := uint64(0)
	if (ucTemp & 0x80) > 0 {
		cpu.PUSHWU(cpu.regPC)
		i += 2
	}
	if (ucTemp & 0x40) > 0 {
		cpu.PUSHWU(cpu.regS)
		i += 2
	}
	if (ucTemp & 0x20) > 0 {
		cpu.PUSHWU(cpu.regY)
		i += 2
	}
	if (ucTemp & 0x10) > 0 {
		cpu.PUSHWU(cpu.regX)
		i += 2
	}
	if (ucTemp & 0x8) > 0 {
		cpu.PUSHBU(cpu.regDP)
		i++
	}
	if (ucTemp & 0x4) > 0 {
		cpu.PUSHBU(cpu.regB)
		i++
	}
	if (ucTemp & 0x2) > 0 {
		cpu.PUSHBU(cpu.regA)
		i++
	}
	if (ucTemp & 0x1) > 0 {
		cpu.PUSHBU(cpu.regCC)
		i++
	}
	cpu.tickCount += i //timing
}

//Pull A, B, CC, DP, D, X, Y, S, or PC from hardware stack
func (cpu *CPU) PULU(ucTemp uint8) {
	i := uint64(0)
	if (ucTemp & 0x1) > 0 {
		cpu.regCC = cpu.PULLBU()
		i++
	}
	if (ucTemp & 0x2) > 0 {
		cpu.regA = cpu.PULLBU()
		i++
	}
	if (ucTemp & 0x4) > 0 {
		cpu.regB = cpu.PULLBU()
		i++
	}
	if (ucTemp & 0x8) > 0 {
		cpu.regDP = cpu.PULLBU()
		i++
	}
	if (ucTemp & 0x10) > 0 {
		cpu.regX = cpu.PULLWU()
		i += 2
	}
	if (ucTemp & 0x20) > 0 {
		cpu.regY = cpu.PULLWU()
		i += 2
	}
	if (ucTemp & 0x40) > 0 {
		cpu.regS = cpu.PULLWU()
		i += 2
	}
	if (ucTemp & 0x80) > 0 {
		cpu.regPC = cpu.PULLWU()
		i += 2
	}
	cpu.tickCount += i //timing
}
