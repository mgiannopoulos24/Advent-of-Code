#include <stdio.h>
#include <stdlib.h>


int max(int a, int b);

int main() {
    FILE *f = fopen("input_level_2.txt", "r"); // Assuming the file is named presents.txt
    if (f == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int l, w, h;
    int totalRibbon = 0;

    // Read each set of dimensions from the file
    while (fscanf(f, "%dx%dx%d", &l, &w, &h) == 3) {
        int ribbonWrap = 2*(l + w + h - max(l, max(w, h))); // Calculate the smallest perimeter
        int ribbonBow = l * w * h; // Calculate the bow length as the volume

        totalRibbon += ribbonWrap + ribbonBow;
    }

    printf("Total feet of ribbon needed: %d\n", totalRibbon);

    fclose(f); // Close the file
    return 0;
}

// Helper function to find the maximum of two numbers
int max(int a, int b) {
    return (a > b) ? a : b;
}
