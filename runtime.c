// runtime.c
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

// ========== FILE I/O ==========
static FILE *output_file = NULL;

void num_open_file_write(const char *filename) {
    char pathbuf[512];
    // Prepend tests/phase5/ to the user‐supplied filename
    snprintf(pathbuf, sizeof(pathbuf), "tests/phase5/%s", filename);

    output_file = fopen(pathbuf, "w");
    if (!output_file) {
        perror("Failed to open file");
        exit(1);
    }
}

void num_write_num(int num) {
    if (output_file) {
        fprintf(output_file, "%d\n", num);
    }
}

void num_write_num_int(int num) {
    if (output_file) {
        fprintf(output_file, "%d\n", num);
    }
}

void num_close_file() {
    if (output_file) {
        fclose(output_file);
        output_file = NULL;
    }
}

// ========== MEMORY MODEL ==========
#define MEM_SIZE (1 << 27)  // 128 MB
static uint8_t *memory = NULL;
static uintptr_t next_free = 0;
static uintptr_t last_base = 0;

void ptr_alloc(long size) {
    if (!memory) {
        memory = malloc(MEM_SIZE);
        if (!memory) {
            perror("malloc failed");
            exit(1);
        }
        next_free = 0;
    }
    if (next_free + (uintptr_t)size > MEM_SIZE) {
        fprintf(stderr, "Out of memory in ptr_alloc\n");
        exit(1);
    }
    last_base = next_free;
    next_free += (uintptr_t)size;
}

int ptr_get_lower() {
    return (int)(last_base & 0xFFFFFFFFU);
}

int ptr_get_upper() {
    return (int)(last_base >> 32);
}

void ptr_free(long addr) {
    (void)addr;
}

int ptr_read_int(long addr) {
    if (addr < 0 || (uintptr_t)addr + sizeof(int) > MEM_SIZE) {
        fprintf(stderr, "Invalid read at %ld\n", addr);
        exit(1);
    }
    int v;
    memcpy(&v, memory + addr, sizeof(v));
    return v;
}

void ptr_write_int(long addr, int value) {
    if (addr < 0 || (uintptr_t)addr + sizeof(int) > MEM_SIZE) {
        fprintf(stderr, "Invalid write at %ld\n", addr);
        exit(1);
    }
    memcpy(memory + addr, &value, sizeof(value));
}

long ptr_add_int(long ptr, int offset) {
    return ptr + (long)offset * 4L;
}

// ========== TIMER ==========
static clock_t start_time, end_time;

void start_timer() {
    start_time = clock();
}

void end_timer() {
    end_time = clock();
}

void timer_print() {
    double duration = (double)(end_time - start_time) / CLOCKS_PER_SEC;
    printf("Execution time: %.4f seconds\n", duration);
}

void timer_write() {
    if (output_file) {
        double duration = (double)(end_time - start_time) / CLOCKS_PER_SEC;
        fprintf(output_file, "Execution time: %.4f seconds\n", duration);
    }
}
