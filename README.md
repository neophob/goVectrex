# General Consumer Electronics Vectrex

## Hardware

- CPU: Motorola 6809 @ 1.5 MHz
- RAM: 1kB
- ROM: 8k
- Cartridge ROM: 32 KB
- Sound: General Instrument AY-3-8912 aka YM2149 (https://en.wikipedia.org/wiki/General_Instrument_AY-3-8910)
- I/O Port: VIA 6522 (https://en.wikipedia.org/wiki/MOS_Technology_6522)

## Misc

- A total of 28 games were released for the Vectrex
- https://en.wikipedia.org/wiki/Vectrex
- https://github.com/jhawthorn/vecx

## GO

- Value receiver makes a copy of the type and pass it to the function. The function stack now holds an equal object but at a different location on memory. That means any changes done on the passed object will remain local to the method. *The original object will remain unchanged.*
- Pointer receiver passes the address of a type to the function. The function stack has a reference to the original object. So any modifications on the passed object will *modify the original object.*
