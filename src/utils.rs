pub fn from_twos_complement(v: u8) -> i8 {
    if v & 0b1000_0000 > 0 {
        -1 * ((v ^ 0b1111_1111) as i8 + 1)
    } else {
        v as i8
    }
}

pub fn unbounded_shr_u8<T>(val: u8, sh: T) -> u8
where
    u32: From<T>,
{
    let val: u32 = val.into();
    let sh: u32 = sh.into();

    return val.checked_shr(sh).unwrap_or(0) as u8;
}
