#include "DebuggerWindow.h"
#include <gtkmm/application.h>

int main(int argc, char *argv[]) {
    auto app = Gtk::Application::create(argc, argv, "org.cuauv.serial.debugger");

    DebuggerWindow window;

    return app->run(window);
}
