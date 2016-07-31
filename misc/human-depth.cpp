/*
 * Temporary human-based depth controller.
 */
#include <iostream>
#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include <stdio.h>

#include <libshm/c/shm.h>

constexpr double increment = 0.1;

int main()
{
    std::cout << "  h     j     k     l" << std::endl;
    printf("-%2.1f  -%2.1f  +%2.1f  +%2.1f\n", 2*increment, increment, increment, 2*increment);

    termios t;
    tcgetattr(STDIN_FILENO, &t);
    t.c_lflag &= ~ICANON;
    t.c_lflag &= ~ECHO;
    tcsetattr(STDIN_FILENO, TCSANOW, &t);

    char c;
    double depth;
    
    shm_init();

    for (;;) {
        c = std::getc(stdin);

        shm_get(dvl, depth, depth);
        switch (c) {
        case 'k': // up
            shm_set(dvl, depth, depth + increment);
            break;
        case 'j': // down
            shm_set(dvl, depth, depth - increment);
            break;
        case 'l': // up!
            shm_set(dvl, depth, depth + 2 * increment);
            break;
        case 'h': // down!
            shm_set(dvl, depth, depth - 2 * increment);
            break;
        default:
            std::cout << "Invalid character." << std::endl;
        }

        shm_get(dvl, depth, depth);

        printf("Depth is now %.2f.\n", depth);
    }

    tcgetattr(STDIN_FILENO, &t);
    t.c_lflag |= ICANON;
    t.c_lflag |= ECHO;
    tcsetattr(STDIN_FILENO, TCSANOW, &t);
    return 0;
}
