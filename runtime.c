// runtime.c
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <errno.h>

// ——————————————————————————————————————————
// globals for ptr_* functions
// ——————————————————————————————————————————
static void *last_allocated = NULL;

void ptr_alloc(long size)
{
    last_allocated = malloc((size_t)size);
    if (!last_allocated)
    {
        perror("malloc");
        exit(1);
    }
}

long ptr_add_int(long base, int offset)
{
    return (long)((uintptr_t)base + offset);
}

void ptr_write_int(long addr, int value)
{
    int *p = (int *)(uintptr_t)addr;
    *p = value;
}

int ptr_read_int(long addr)
{
    int *p = (int *)(uintptr_t)addr;
    return *p;
}

int ptr_get_lower(void)
{
    uintptr_t p = (uintptr_t)last_allocated;
    return (int)(p & 0xFFFFFFFF);
}

int ptr_get_upper(void)
{
    uintptr_t p = (uintptr_t)last_allocated;
    return (int)((p >> 32) & 0xFFFFFFFF);
}

void ptr_free(long addr)
{
    void *p = (void *)(uintptr_t)addr;
    free(p);
}

// ——————————————————————————————————————————
// globals for num_* file I/O
// ——————————————————————————————————————————
static FILE *out_file = NULL;

void num_open_file_write(const char *filename)
{
    // ensure the test harness directory exists
    if (mkdir("tests/phase5/output", 0755) && errno != EEXIST)
    {
        perror("mkdir tests/phase5/output");
        exit(1);
    }
    out_file = fopen(filename, "w");
    if (!out_file)
    {
        perror("fopen");
        exit(1);
    }
}

void num_write_num(double x)
{
    if (!out_file)
    {
        fputs("File not open\n", stderr);
        exit(1);
    }
    fprintf(out_file, "%f\n", x);
}

void num_write_num_int(int x)
{
    if (!out_file)
    {
        fputs("File not open\n", stderr);
        exit(1);
    }
    fprintf(out_file, "%d\n", x);
}

void num_close_file(void)
{
    if (out_file)
    {
        fclose(out_file);
        out_file = NULL;
    }
}

// ——————————————————————————————————————————
// timer_* functions
// ——————————————————————————————————————————
static struct timeval tv_start, tv_end;
static double elapsed_time;

void start_timer(void)
{
    gettimeofday(&tv_start, NULL);
}

void end_timer(void)
{
    gettimeofday(&tv_end, NULL);
    elapsed_time = (tv_end.tv_sec - tv_start.tv_sec) + (tv_end.tv_usec - tv_start.tv_usec) / 1e6;
}

void timer_print(void)
{
    printf("Elapsed time: %f seconds\n", elapsed_time);
}

void timer_write(void)
{
    // write timing out alongside mergesort output if you like:
    if (mkdir("tests/phase5/output", 0755) && errno != EEXIST)
    {
        perror("mkdir tests/phase5/output");
        exit(1);
    }
    FILE *tf = fopen("tests/phase5/output/timer.txt", "w");
    if (!tf)
    {
        perror("fopen timer");
        exit(1);
    }
    fprintf(tf, "%f\n", elapsed_time);
    fclose(tf);
}
