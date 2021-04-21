#include <thread>
#include <iostream>

using namespace std;

int main() {
    cout << thread::hardware_concurrency() << endl;
    return EXIT_SUCCESS;
}
