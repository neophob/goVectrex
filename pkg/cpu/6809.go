package cpu

import "../logger"

const (
	flagCarry     uint8 = (1 << 0)
	flagOverflow  uint8 = (1 << 1)
	flagZero      uint8 = (1 << 2)
	flagNegative  uint8 = (1 << 3)
	flagIrqMask   uint8 = (1 << 4)
	flagHalfCarry uint8 = (1 << 5)
	flagFirqMask  uint8 = (1 << 6)
	flagEntire    uint8 = (1 << 7)

	vectorReset uint16 = 0xFFFE
	vectorFirq  uint16 = 0xFFF6
	vectorIrq   uint16 = 0xFFF8
	vectorNmi   uint16 = 0xFFFC
	vectorSwi   uint16 = 0xFFFA
	vectorSwi2  uint16 = 0xFFF4
	vectorSwi3  uint16 = 0xFFF2
)

// MemoryAccess provides and interface to read and write function to access the memory
type MemoryAccess interface {
	read(offset uint16) uint8
	write(offset uint16, data uint8)
}

// CPU is the encapsulated data structure for this CPU
type CPU struct {
	memory MemoryAccess

	tickCount   uint64
	irqPending  bool
	firqPending bool
	missedIRQ   uint64
	missedFIRQ  uint64
	irqCount    uint64
	firqCount   uint64
	nmiCount    uint64

	regA  uint8
	regB  uint8
	regX  uint16
	regY  uint16
	regU  uint16
	regS  uint16
	regPC uint16
	regCC uint8
	regDP uint8
}

// Build a new 6809 CPU instance
func Build(memory MemoryAccess) *CPU {
	cpu := &CPU{memory: memory}
	cpu.reset()
	return cpu
}

func (cpu *CPU) reset() {
	cpu.tickCount = 0
	cpu.irqPending = false
	cpu.firqPending = false
	cpu.missedIRQ = 0
	cpu.missedFIRQ = 0
	cpu.irqCount = 0
	cpu.firqCount = 0
	cpu.nmiCount = 0

	cpu.regCC = flagIrqMask | flagFirqMask
	cpu.regPC = cpu.readWord(vectorReset)
	logger.Debug("exec reset 0x%X", cpu.regPC)
}

func (cpu *CPU) fetch() uint8 {
	cpu.regPC++
	return cpu.read(cpu.regPC)
}

func (cpu *CPU) fetch16() uint16 {
	cpu.regPC++
	v1 := uint16(cpu.read(cpu.regPC))
	cpu.regPC++
	v2 := uint16(cpu.read(cpu.regPC))
	return (v1 << 8) + v2
}

func (cpu *CPU) write(offset uint16, data uint8) {
	cpu.memory.write(offset, data)
}

func (cpu *CPU) read(offset uint16) uint8 {
	return cpu.memory.read(offset)
}

func (cpu *CPU) writeWord(addr, data uint16) {
	cpu.write(addr, uint8((data>>8)&0xff))
	cpu.write((addr+1)&0xFFFF, uint8(data&0xff))
}

func (cpu *CPU) readWord(addr uint16) uint16 {
	var v1 = (uint16)(cpu.read(addr))
	var v2 = (uint16)(cpu.read((addr + 1) & 0xFFFF))
	return v1<<8 + v2
}

// Step execute next instruction, return executed ticks
func (cpu *CPU) Step() uint64 {
	oldTickCount := cpu.tickCount

	if cpu.firqPending {
		if (cpu.regCC & flagFirqMask) == 0 {
			cpu.firqPending = false
			cpu.firqCount++
			cpu._executeFirq()
			return cpu.tickCount - oldTickCount
		}
		cpu.missedFIRQ++
	}

	if cpu.irqPending {
		if (cpu.regCC & flagIrqMask) == 0 {
			cpu.irqPending = false
			cpu.irqCount++
			cpu._executeIrq()
			return cpu.tickCount - oldTickCount
		}
		cpu.missedIRQ++
	}

	return cpu.RunOpCode()
}

// GetState returns the internal state of the CPU
func (cpu *CPU) GetState() CPU {
	return CPU{
		tickCount:   cpu.tickCount,
		irqPending:  cpu.irqPending,
		firqPending: cpu.firqPending,
		missedIRQ:   cpu.missedIRQ,
		missedFIRQ:  cpu.missedFIRQ,
		irqCount:    cpu.irqCount,
		firqCount:   cpu.firqCount,
		nmiCount:    cpu.nmiCount,

		regA:  cpu.regA,
		regB:  cpu.regB,
		regX:  cpu.regX,
		regY:  cpu.regY,
		regU:  cpu.regU,
		regS:  cpu.regS,
		regCC: cpu.regCC,
		regPC: cpu.regPC,
		regDP: cpu.regDP,
	}
}

func (cpu *CPU) _executeFirq() {
	// TODO check if CWAI is pending
	logger.Debug("EXEC_FIRQ, count %d", cpu.tickCount)
	// clear ENTIRE flag to this.regCC, used for RTI
	cpu.regCC &= ^flagEntire
	cpu.PUSHW(cpu.regPC)
	cpu.PUSHB(cpu.regCC)
	//Disable interrupts, Set F,I
	cpu.regCC |= flagIrqMask | flagFirqMask
	cpu.regPC = cpu.readWord(vectorFirq)
	cpu.tickCount += 10
}

func (cpu *CPU) _executeIrq() {
	// TODO check if CWAI is pending
	logger.Debug("EXEC_IRQ, count %d", cpu.tickCount)
	// set ENTIRE flag to this.regCC, used for RTI
	cpu.regCC |= flagEntire
	cpu.PUSHW(cpu.regPC)
	cpu.PUSHW(cpu.regU)
	cpu.PUSHW(cpu.regY)
	cpu.PUSHW(cpu.regX)
	cpu.PUSHB(cpu.regDP)
	cpu.PUSHB(cpu.regB)
	cpu.PUSHB(cpu.regA)
	cpu.PUSHB(cpu.regCC)
	// Disable interrupts, Set I
	cpu.regCC |= flagIrqMask
	cpu.regPC = cpu.readWord(vectorIrq)
	cpu.tickCount += 19
}
