Rust type-level programming
===========================






Limitations
-----------



### No equality for type-level const chars

- [Is it possible to chain overlapping trait implementations, giving certain ones precedence over others?](https://www.reddit.com/r/rust/comments/lej8ee/is_it_possible_to_chain_overlapping_trait/)
- [RFC: impl specialization](https://github.com/rust-lang/rfcs/pull/1210)

    trait Eq<Other> {
      type A;
      type B;
      type Output;
    }


    enum Ch<const CH: char> {}

    // Overlapped instances
    impl<const CH: char>
      Eq<Ch<CH>> for Ch<CH> {
      type A = Ch<CH>;
      type B = Ch<CH>;
      type Output = True;
    }

    impl<const CH1: char, const CH2: char>
      Eq<Ch<CH2>> for Ch<CH1> {
      type A = Ch<CH1>;
      type B = Ch<CH2>;
      type Output = False;
    }
