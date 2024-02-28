#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *f = fopen("input_level_2.txt", "r"); // Assuming the file is named presents.txt
    if (f == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int l, w, h;
    int totalPaper = 0;

    // Read each set of dimensions from the file
    while (fscanf(f, "%dx%dx%d", &l, &w, &h) == 3) {
        int surfaceArea = 2*l*w + 2*w*h + 2*h*l;
        int slack = l*w; // Initialize slack to the area of the first side

        // Check if any other side area is smaller and use it as slack
        if (w*h < slack) slack = w*h;
        if (h*l < slack) slack = h*l;

        totalPaper += surfaceArea + slack;
    }

    printf("Total square feet of wrapping paper needed: %d\n", totalPaper);

    fclose(f); // Close the file
    return 0;
}
