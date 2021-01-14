package cpu

import (
	"testing"
)

type Memory struct {
	data [1024]uint8
}

func (memory *Memory) read(offset uint16) uint8 {
	if offset > 1023 {
		switch offset {
		case 0xFFFE:
			return 0xAA
		case 0xFFFF:
			return 0xAA

		}
		return 0
	}
	return memory.data[offset]
}
func (memory *Memory) write(offset uint16, data uint8) {
	if offset > 1023 {
		return
	}
	memory.data[offset] = data
}

func TestResetCPU(t *testing.T) {
	memory := &Memory{}
	cpu := Build(memory)

	cpuState := cpu.GetState()
	if cpuState.regPC != 0xAAAA {
		t.Errorf("cpuState.regPC is invalid after reset, 0x%X", cpuState.regPC)
	}
}
