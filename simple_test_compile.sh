# Simple script to compile the test
# Just do ./simple_test_compile.sh in the terminal
# You may need to do `chmod +x simple_test_compile.sh` before this will work
gfortran mod_precision.F90 mod_match.F90 simple_test.F90 -O3 -o test
