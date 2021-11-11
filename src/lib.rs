use std::ops::{BitAnd, BitOr, BitXor};

#[derive(Clone, Copy, Debug, Default)]
pub struct RippleCarryAdder {
    adders: [FullAdder; 8],
}

impl RippleCarryAdder {
    pub fn add(&mut self, a: u8, b: u8) -> Result<u8, &'static str> {
        for i in 0..self.adders.len() {
            /* The bits are extracted from the narrow end by shifting right `i` positions,
             * where 0 <= i < 8. On first iteration, i == 0, and nothing gets shifted.
             * However, each time through the loop, the number of shifted positions (`i`)
             * increases by 1 until it reaches 7. Consequently, the previously extracted bit is
             * discarded and the bit we want to extract next comes to its place, i.e., becomes LSB.
             * By bitwise ANDing the resulting number with 1 (mask), the individual bit at the end
             * is effectively extracted from the number - masking out the rest.
             */
            let bit1 = Bit((a >> i) & 1);
            let bit2 = Bit((b >> i) & 1);
            /* The adders are split into two halves at the position following the current adder (`i`),
             * which places the current adder in the last position in the left half, while the next
             * adder becomes the first one in the right half.
             */
            let (left, right) = self.adders.split_at_mut(i + 1);
            let current_adder = &mut left[i];
            current_adder.add(bit1, bit2);
            match right.get_mut(0) {
                Some(next_adder) => {
                    // Pass the carry bit to the next adder if the current adder is not the last one.
                    next_adder.carry_in = current_adder.carry_out;
                }
                None => {
                    if current_adder.carry_out == Bit(1) {
                        /* Having no adders after the current one means that current
                        adder is the last one. Furthermore, if the carry out for that
                        adder is 1, we have an overflow.
                         */
                        return Err("Overflow.");
                    }
                }
            }
        }
        Ok(self.get_result())
    }

    fn get_result(&self) -> u8 {
        /* Since the addition was done starting from the narrow end,
         * when building the result, the adders are reversed so that
         * the adder holding the MSB comes first. The bits are packed
         * back into the result by shifting left by 1 each time through
         * the loop and ORing the result with the sum of an individual adder.
         */
        let mut result = 0;
        for adder in self.adders.iter().rev() {
            result <<= 1;
            result |= adder.sum.0;
        }
        result
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct FullAdder {
    carry_in: Bit,
    carry_out: Bit,
    sum: Bit,
    halfadder1: HalfAdder,
    halfadder2: HalfAdder,
}

impl FullAdder {
    fn add(&mut self, a: Bit, b: Bit) {
        self.halfadder1.add(a, b);
        self.halfadder2.add(self.halfadder1.sum, self.carry_in);
        self.sum = self.halfadder2.sum;
        self.carry_out = self.halfadder1.carry_out | self.halfadder2.carry_out;
    }
}

trait WithCarryIn {
    fn with_carry_in(carry_in: Bit) -> FullAdder;
}

#[cfg(test)]
impl WithCarryIn for FullAdder {
    fn with_carry_in(carry_in: Bit) -> Self {
        Self {
            carry_in,
            carry_out: Bit(0),
            sum: Bit(0),
            halfadder1: HalfAdder::default(),
            halfadder2: HalfAdder::default(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct HalfAdder {
    carry_out: Bit,
    sum: Bit,
}

impl HalfAdder {
    fn add(&mut self, a: Bit, b: Bit) {
        self.carry_out = a & b;
        self.sum = a ^ b;
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
struct Bit(u8);

impl BitAnd for Bit {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Bit(self.0 & rhs.0)
    }
}

impl BitOr for Bit {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Bit(self.0 | rhs.0)
    }
}

impl BitXor for Bit {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self {
        Bit(self.0 ^ rhs.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest(
        a,
        b,
        expected,
        case(4, 4, Ok(8)),
        case(128, 127, Ok(255)),
        case(128, 128, Err("Overflow."))
    )]
    fn ripple_carry_adder(a: u8, b: u8, expected: Result<u8, &'static str>) {
        let mut adder = RippleCarryAdder::default();
        let result = adder.add(a, b);
        assert_eq!(result, expected);
    }

    #[rstest(
        a,
        b,
        carry_in,
        expected_carry_out,
        expected_sum,
        case(Bit(0), Bit(0), Bit(0), Bit(0), Bit(0)),
        case(Bit(0), Bit(0), Bit(1), Bit(0), Bit(1)),
        case(Bit(0), Bit(1), Bit(0), Bit(0), Bit(1)),
        case(Bit(0), Bit(1), Bit(1), Bit(1), Bit(0)),
        case(Bit(1), Bit(0), Bit(0), Bit(0), Bit(1)),
        case(Bit(1), Bit(0), Bit(1), Bit(1), Bit(0)),
        case(Bit(1), Bit(1), Bit(0), Bit(1), Bit(0)),
        case(Bit(1), Bit(1), Bit(1), Bit(1), Bit(1))
    )]
    fn full_add(a: Bit, b: Bit, carry_in: Bit, expected_carry_out: Bit, expected_sum: Bit) {
        let mut adder = FullAdder::with_carry_in(carry_in);
        adder.add(a, b);
        assert_eq!(adder.carry_out, expected_carry_out);
        assert_eq!(adder.sum, expected_sum);
    }

    #[rstest(
        a,
        b,
        expected_carry_out,
        expected_sum,
        case(Bit(0), Bit(0), Bit(0), Bit(0)),
        case(Bit(0), Bit(1), Bit(0), Bit(1)),
        case(Bit(1), Bit(0), Bit(0), Bit(1)),
        case(Bit(1), Bit(1), Bit(1), Bit(0))
    )]
    fn half_adder(a: Bit, b: Bit, expected_carry_out: Bit, expected_sum: Bit) {
        let mut adder = HalfAdder::default();
        adder.add(a, b);
        assert_eq!(adder.carry_out, expected_carry_out);
        assert_eq!(adder.sum, expected_sum);
    }
}
