#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 100
#define MAX_REPLACEMENTS 50
#define MAX_MOLECULE_LENGTH 1000

typedef struct {
    char source[MAX_LINE_LENGTH];
    char target[MAX_LINE_LENGTH];
} Replacement;

typedef struct Node {
    char molecule[MAX_MOLECULE_LENGTH];
    int steps;
    struct Node *next;
} Node;

void insert(Node **head, const char *molecule, int steps) {
    Node *new_node = (Node *)malloc(sizeof(Node));
    strcpy(new_node->molecule, molecule);
    new_node->steps = steps;
    new_node->next = *head;
    *head = new_node;
}

int contains(Node *head, const char *molecule) {
    Node *current = head;
    while (current != NULL) {
        if (strcmp(current->molecule, molecule) == 0) {
            return 1;
        }
        current = current->next;
    }
    return 0;
}

void free_list(Node *head) {
    Node *tmp;
    while (head != NULL) {
        tmp = head;
        head = head->next;
        free(tmp);
    }
}

int bfs(Replacement *replacements, int rep_count, const char *target) {
    Node *queue = NULL;
    insert(&queue, "e", 0);

    Node *visited = NULL;
    insert(&visited, "e", 0);

    while (queue != NULL) {
        Node *current = queue;
        queue = queue->next;

        if (strcmp(current->molecule, target) == 0) {
            int steps = current->steps;
            free_list(queue);
            free_list(visited);
            free(current);
            return steps;
        }

        int mol_len = strlen(current->molecule);
        for (int i = 0; i < mol_len; i++) {
            for (int j = 0; j < rep_count; j++) {
                int src_len = strlen(replacements[j].source);
                if (strncmp(&current->molecule[i], replacements[j].source, src_len) == 0) {
                    char new_molecule[MAX_MOLECULE_LENGTH];
                    strncpy(new_molecule, current->molecule, i);
                    new_molecule[i] = '\0';
                    strcat(new_molecule, replacements[j].target);
                    strcat(new_molecule, &current->molecule[i + src_len]);

                    if (!contains(visited, new_molecule)) {
                        insert(&queue, new_molecule, current->steps + 1);
                        insert(&visited, new_molecule, current->steps + 1);
                    }
                }
            }
        }

        free(current);
    }

    free_list(visited);
    return -1; // If no solution is found
}

int main() {
    FILE *file = fopen("input_level_19.txt", "r");
    if (file == NULL) {
        perror("Failed to open file");
        return 1;
    }

    Replacement replacements[MAX_REPLACEMENTS];
    int rep_count = 0;
    char line[MAX_LINE_LENGTH];

    while (fgets(line, sizeof(line), file)) {
        if (strchr(line, '=') == NULL) {
            break;
        }
        sscanf(line, "%s => %s", replacements[rep_count].source, replacements[rep_count].target);
        rep_count++;
    }

    char target_molecule[MAX_MOLECULE_LENGTH];
    fgets(target_molecule, sizeof(target_molecule), file);
    target_molecule[strcspn(target_molecule, "\n")] = 0;  // Remove newline character

    fclose(file);

    int steps = bfs(replacements, rep_count, target_molecule);
    if (steps != -1) {
        printf("Fewest number of steps to make the medicine: %d\n", steps);
    } else {
        printf("No solution found.\n");
    }

    return 0;
}
