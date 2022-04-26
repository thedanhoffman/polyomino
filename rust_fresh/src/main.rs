fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    mod explicit {
        fn a(n: u64) -> u64 {
            if n == 0 {
                1
            } else {
                a(n-1) + c(n-1) + d(n-1) + e(n-1) + g(n-1)
            }
        }
       
        fn b(n: u64) -> u64 {
            if n == 0 {
                0
            } else {
                c(n-1)
            }
        }
       
        fn c(n: u64) -> u64 {
            if n == 0 {
                0
            } else {
                d(n-1)
            }
        }

        fn d(n: u64) -> u64 {
            if n == 0 {
                0
            } else {
                b(n-1) + g(n-1)
            }
        }

        fn e(n: u64) -> u64 {
            if n == 0 {
                0
            } else {
                f(n-1)
            }
        }

        fn f(n: u64) -> u64 {
            if n == 0 {
                0
            } else {
                a(n-1)
            }
        }

        fn g(n: u64) -> u64 {
            if n == 0 {
                0
            } else {
                2*a(n-1)
            }
        }

        #[test]
        fn test_explicit_trominoes() {
            let mut compute = |n: u64| -> u64 {
                a(n)
            };

            for i in (1..=10) {
                println!("3x{}: {}", i, compute(i));
            }

            assert_eq![compute(1), 1];
            assert_eq![compute(2), 3];
            assert_eq![compute(3), 10];
            assert_eq![compute(4), 23];
            assert_eq![compute(5), 62];
            assert_eq![compute(6), 170];
            assert_eq![compute(7), 441];
            assert_eq![compute(8), 1173];
            assert_eq![compute(9), 3127];
            assert_eq![compute(10), 8266];

            // NOTE: "some states are not reachable within the first N rows, so the actual
            // number of states involved in solving the NxN board was slightly lower).
            // I don't believe this is relevant for the 3x3 case

            // NOTE: does the 3x3 case have additional symmetries simplified away because
            // of diagonal symmetries?
        
            // "above row one" ... "by the 9th row" makes me think it is evaluating
            // the slice of the top row *from above* (meaning the slice is the boundary),
            // so any implicit summations are out
        
            // NOTE: the area in the boxes in Figure 4 is the total amount of area currently
            // drawn
        }
    }
}
