#include <stdio.h>

long generate_code(int row, int col) {
    long code = 20151125;
    for (int i = 1; i < row + col - 1; ++i) {
        code = (code * 252533) % 33554393;
    }
    return code;
}

long find_code(int row, int col) {
    int code_row = 1, code_col = 1;
    long code = 20151125;
    while (code_row != row || code_col != col) {
        if (code_row == 1) {
            code_row = code_col + 1;
            code_col = 1;
        } else {
            code_row -= 1;
            code_col += 1;
        }
        code = (code * 252533) % 33554393;
    }
    return code;
}

int main() {
    int row = 2981;
    int col = 3075;
    long code = find_code(row, col);
    printf("The code at row %d, column %d is: %ld\n", row, col, code);
    return 0;
}
