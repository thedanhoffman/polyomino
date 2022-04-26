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
                d(n-1) + g(n-1)
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
            assert_eq![a(1), 1];
            assert_eq![a(2), 3];
            assert_eq![a(3), 10];
            assert_eq![a(4), 23];
            assert_eq![a(5), 62];
            assert_eq![a(6), 170];
            assert_eq![a(7), 441];
            assert_eq![a(8), 1173];
            assert_eq![a(9), 3127];
            assert_eq![a(10), 8266];
        }
    }
}
