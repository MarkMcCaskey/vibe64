/// Exception codes for the VR4300 (stored in COP0 Cause register bits [6:2]).
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum ExceptionCode {
    Interrupt = 0,
    TlbModification = 1,
    TlbLoad = 2,
    TlbStore = 3,
    AddressErrorLoad = 4,
    AddressErrorStore = 5,
    BusErrorInstruction = 6,
    BusErrorData = 7,
    Syscall = 8,
    Breakpoint = 9,
    ReservedInstruction = 10,
    CoprocessorUnusable = 11,
    Overflow = 12,
    Trap = 13,
    FloatingPoint = 15,
    Watch = 23,
}
