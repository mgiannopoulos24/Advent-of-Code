#include <stdio.h>
#include <stdlib.h>
#include "cJSON.h"
#include <string.h>

long sum_numbers_in_json(cJSON *json);

long process_array(cJSON *array) {
    long sum = 0;
    cJSON *current_element = NULL;
    cJSON_ArrayForEach(current_element, array) {
        sum += sum_numbers_in_json(current_element);
    }
    return sum;
}

int contains_red(cJSON *object) {
    cJSON *current_element = NULL;
    cJSON_ArrayForEach(current_element, object) {
        if (cJSON_IsString(current_element) && strcmp(current_element->valuestring, "red") == 0) {
            return 1;
        }
    }
    return 0;
}

long sum_numbers_in_json(cJSON *json) {
    long sum = 0;
    if (cJSON_IsNumber(json)) {
        sum = json->valueint; // or json->valuedouble if you're dealing with doubles
    } else if (cJSON_IsArray(json)) {
        sum = process_array(json);
    } else if (cJSON_IsObject(json)) {
        if (!contains_red(json)) {
            cJSON *child = json->child;
            while (child) {
                sum += sum_numbers_in_json(child);
                child = child->next;
            }
        }
    }
    return sum;
}

char *read_file_to_string(const char *filename) {
    FILE *f = fopen(filename, "rb");
    if (f == NULL) return NULL;

    fseek(f, 0, SEEK_END);
    long length = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *data = (char *)malloc(length + 1);
    fread(data, 1, length, f);
    fclose(f);

    data[length] = '\0';
    return data;
}

int main() {
    char *file_content = read_file_to_string("input_level_12.json");
    if (file_content == NULL) {
        printf("Failed to read file\n");
        return 1;
    }

    cJSON *json = cJSON_Parse(file_content);
    if (json == NULL) {
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stderr, "Error before: %s\n", error_ptr);
        }
        free(file_content);
        return 1;
    }

    long total_sum = sum_numbers_in_json(json);
    printf("Total sum of all numbers, excluding red objects: %ld\n", total_sum);

    cJSON_Delete(json);
    free(file_content);

    return 0;
}
