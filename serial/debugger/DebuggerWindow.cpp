#include "DebuggerWindow.h"

#include <gtkmm/dialog.h> // for Gtk::RESPONSE_CLOSE
#include <gdk/gdkkeysyms.h>

#include <iostream>

using namespace cuauv::serial;

DebuggerWindow::DebuggerWindow() :
    m_outerBox(Gtk::ORIENTATION_VERTICAL),
    m_hardKillFlag("Not Hard Killed", "Hard Killed", "Hard Kill Unknown"),
    m_connectedFlag("Connected", "Disconnected"),
    m_portStatusFlag("Port OK", "Port Down"),
    m_resetButton("Reset Device"),
    m_scanButton("Update Ports"),
    m_portSelect(true), // enables arbitrary text entry
    m_connectButton("Change Port"),
    m_writeVarWidget(true),
    m_scanActive(false),
    m_hardKill(true),
    m_hardKillValid(false),
    m_softKill(true),
    m_devInfo(nullptr),
    m_manager(nullptr)
{
    set_border_width(10);

    m_connectButton.signal_clicked().connect(sigc::mem_fun(*this, &DebuggerWindow::on_change_port));
    m_portSelect.get_entry()->signal_activate().connect(sigc::mem_fun(*this, &DebuggerWindow::on_select_activate));
    m_portSelect.signal_changed().connect(sigc::mem_fun(*this, &DebuggerWindow::on_select_modified));
    m_scanButton.signal_clicked().connect(sigc::mem_fun(*this, &DebuggerWindow::on_scan_ports));
    m_softKillButton.signal_clicked().connect(sigc::mem_fun(*this, &DebuggerWindow::on_softkill_changed));
    m_resetButton.signal_clicked().connect(sigc::mem_fun(*this, &DebuggerWindow::on_reset_device));
    m_killDispatcher.connect(sigc::mem_fun(*this, &DebuggerWindow::on_kill_changed));
    m_connectDispatcher.connect(sigc::mem_fun(*this, &DebuggerWindow::on_connection_changed));
    m_portDownDispatcher.connect(sigc::mem_fun(*this, &DebuggerWindow::on_port_down));
    m_messageDispatcher.connect(sigc::mem_fun(*this, &DebuggerWindow::on_show_message));
    m_readVarDispatcher.connect(sigc::mem_fun(*this, &DebuggerWindow::on_vars_read));

    m_nameLabel.set_text("Name: No Device");
    m_statusGrid.attach(m_nameLabel, 0, 0, 1, 1);
    m_typeLabel.set_text("Type: No Device");
    m_statusGrid.attach(m_typeLabel, 0, 1, 1, 1);

    m_statusFlagGrid.attach(m_hardKillFlag, 0, 0, 2, 1);
    m_statusFlagGrid.attach(m_connectedFlag, 0, 1, 1, 1);
    m_statusFlagGrid.attach(m_portStatusFlag, 1, 1, 1, 1);

    m_statusFlagGrid.set_hexpand();
    m_statusFlagGrid.set_halign(Gtk::ALIGN_CENTER);
    m_statusFlagGrid.set_valign(Gtk::ALIGN_FILL);
    m_statusFlagGrid.set_column_homogeneous(true);
    m_statusFlagGrid.set_row_homogeneous(true);
    m_statusGrid.attach(m_statusFlagGrid, 1, 0, 1, 2);

    m_statusGrid.attach(m_resetButton, 2, 0, 1, 1);
    m_softKillButton.set_label("Soft Killed");
    m_softKillButton.set_sensitive(false);
    m_statusGrid.attach(m_softKillButton, 3, 0, 2, 1);

    m_statusGrid.attach(m_scanButton, 2, 1, 1, 1);
    m_portSelect.set_button_sensitivity(Gtk::SENSITIVITY_AUTO);
    m_statusGrid.attach(m_portSelect, 3, 1, 1, 1);
    m_statusGrid.attach(m_connectButton, 4, 1, 1, 1);

    m_statusGrid.set_vexpand(false);
    m_statusGrid.set_margin_bottom(5);
    m_statusGrid.set_row_spacing(2);
    m_statusGrid.set_column_spacing(2);

    static_cast<Gtk::Container*>(m_messageBar.get_content_area())->add(m_messageLabel);
    m_messageBar.set_show_close_button();
    m_messageBar.set_message_type(Gtk::MESSAGE_ERROR);
    m_messageBar.signal_response().connect(sigc::mem_fun(*this, &DebuggerWindow::on_message_response));

    m_outerBox.pack_start(m_statusGrid, Gtk::PACK_SHRINK);
    m_outerBox.pack_start(m_messageBar, Gtk::PACK_SHRINK);
    m_outerBox.pack_start(m_readVarWidget);
    m_outerBox.pack_start(m_writeVarWidget);

    add(m_outerBox);
 
    m_outerBox.show_all();
    m_messageBar.hide();

    // initialize the UI state
    on_kill_changed();
    on_connection_changed();
    on_port_down();

    // Set up the libserial logging hook
    {
        using namespace std::placeholders;
        Log::setLogger(std::bind(&DebuggerWindow::onLog, this, _1, _2, _3));
    }

    // Perform an initial port scan
    on_scan_ports();
}

DebuggerWindow::~DebuggerWindow() {
    kill_manager();
}

void DebuggerWindow::kill_manager() {
    if (m_manager) {
        m_manager->disconnect();
        m_manager = nullptr;
        on_port_down();
    }
}

bool DebuggerWindow::set_port(const std::string& port) {
    kill_manager();
    try {
        using namespace std::placeholders;
        m_manager = std::make_shared<Manager>(port,
                std::bind(&DebuggerWindow::onConnected, this, _1, _2),
                std::bind(&DebuggerWindow::onPortFailed, this, _1, _2));
        m_portStatusFlag.set_state(true);
        m_manager->start();
        m_currentMessage = "";
        hide_error();
        return true;
    } catch (std::exception& e) {
        show_error(e.what());
        m_manager = nullptr;
        return false;
    }
}

void DebuggerWindow::on_port_down() {
    m_portStatusFlag.set_state(false);
    if (m_currentMessage != "") {
        show_error(m_currentMessage);
    }
}

void DebuggerWindow::on_change_port() {
    auto port = m_portSelect.get_entry_text();
    kill_manager();
    set_port(port);
}

void DebuggerWindow::on_select_activate() {
    // Programmatically changing the selected port during a scan
    // generates a spurious select activation. Ignore events during a scan,
    // since the user can't input anything during a scan anyways
    if (!m_scanActive) {
        on_change_port();
    }
}

void DebuggerWindow::on_select_modified() {
    // Ignore change events while the text entry is focused, it's probably the user typing
    if (!m_scanActive && !m_portSelect.get_entry()->is_focus()) {
        on_change_port();
    }
}

void DebuggerWindow::on_scan_ports() {
    m_scanActive = true;
    m_portSelect.remove_all();
    kill_manager();
    bool attachedPort = false;
    for (auto port : Manager::listPorts()) {
        m_portSelect.append(port);
        if (!attachedPort) {
            attachedPort = set_port(port);
            m_portSelect.set_active_text(port);
        }
    }
    if (!attachedPort) {
        m_portSelect.get_entry()->set_text("");
    }
    m_scanActive = false;
}

void DebuggerWindow::on_kill_changed() {
    std::lock_guard<std::mutex> lck(m_lock);
    m_hardKillFlag.set_state(!m_hardKill);
    m_hardKillFlag.set_active(m_hardKillValid);
}

void DebuggerWindow::on_softkill_changed() {
    if (m_softKillButton.get_active()) {
        // unkilled
        m_softKillButton.set_label("Unkilled");
        m_softKill = false;
    } else {
        m_softKillButton.set_label("Soft Killed");
        m_softKill = true;
    }
}

void DebuggerWindow::on_reset_device() {
    if (m_manager) {
        m_manager->resetDevice();
        m_manager->start();
    }
}

void DebuggerWindow::on_connection_changed() {
    std::lock_guard<std::mutex> lck(m_lock);
    if (m_devInfo) {
        m_connectedFlag.set_state(true);
        m_nameLabel.set_text("Name: " + m_devInfo->name());
        m_typeLabel.set_text("Type: " + m_devInfo->type());
        m_softKillButton.set_sensitive(true);
        m_softKillButton.set_active(false);
        m_resetButton.set_sensitive(true);
        on_softkill_changed();

        m_manager->submitRead(m_devInfo->readVariables());

        auto readDefaults = m_devInfo->readDefaults();
        std::unordered_set<BoundVariable> boundVars;
        for (auto var : m_devInfo->readVariables()) {
            auto it = readDefaults.find(var.name());
            if (it != readDefaults.end()) {
                boundVars.insert(it->second);
            } else {
                if (var.type() == Variable::Type::FLOAT) {
                    boundVars.insert(var.bind(0.0f));
                } else {
                    boundVars.insert(var.bind(0));
                }
            }
        }
        m_readVarWidget.setVars(m_manager, boundVars);

        auto writeDefaults = m_devInfo->writeDefaults();
        boundVars.clear();
        for (auto var : m_devInfo->writeVariables()) {
            auto it = writeDefaults.find(var.name());
            if (it != writeDefaults.end()) {
                boundVars.insert(it->second);
            } else {
                if (var.type() == Variable::Type::FLOAT) {
                    boundVars.insert(var.bind(0.0f));
                } else {
                    boundVars.insert(var.bind(0));
                }
            }
        }
        m_manager->submitWrite(boundVars);
        m_writeVarWidget.setVars(m_manager, boundVars);

    } else {
        m_connectedFlag.set_state(false);
        m_nameLabel.set_text("Name: No Device");
        m_typeLabel.set_text("Type: No Device");
        m_softKillButton.set_label("No Device");
        m_softKillButton.set_sensitive(false);
        m_resetButton.set_sensitive(false);
        m_hardKillValid = false;
        m_hardKillFlag.set_active(false);
        m_softKill = true;

        m_readVarWidget.clear();
        m_writeVarWidget.clear();
    }
}

void DebuggerWindow::on_vars_read() {
    std::lock_guard<std::mutex> lck(m_lock);
    if (m_devInfo) {
        // check that devInfo is still valid, just in case Gtk changed the signal order
        m_readVarWidget.updateVars(m_pendingReadVars);
    }
    m_pendingReadVars.clear();
}

bool DebuggerWindow::on_select_key_down(GdkEventKey* event) {
    if (event->keyval == GDK_KEY_Return) {
        on_change_port();
        return true;
    }
    return false;
}

void DebuggerWindow::on_show_message() {
    show_error(m_currentMessage);
}

void DebuggerWindow::show_error(const Glib::ustring& error) {
    m_messageLabel.set_text(error);
    m_messageBar.show();
    std::cout << error << std::endl;
}

void DebuggerWindow::hide_error() {
    m_messageBar.hide();
}    

void DebuggerWindow::on_message_response(int response) {
    if (response == Gtk::RESPONSE_CLOSE) {
        hide_error();
    }
}

std::shared_ptr<DeviceCallbacks> DebuggerWindow::onConnected(Manager* mgr, std::shared_ptr<DeviceInfo> devInfo) {
    std::lock_guard<std::mutex> lck(m_lock);
    (void) mgr;
    m_devInfo = devInfo;
    m_connectDispatcher.emit();
    return std::make_shared<DebuggerCallbacks>(this);
}

void DebuggerWindow::onLog(Log::Level level, const std::string& component, const std::string& msg) {
    std::string prefix;
    switch (level) {
        case Log::Level::INFO:
            prefix = "[INFO] ";
            break;
        case Log::Level::WARN:
            prefix = "[WARN] ";
            break;
        case Log::Level::ERROR:
            prefix = "[ERROR] ";
            break;
        default:
            prefix = "";
    }

    std::lock_guard<std::mutex> lck(m_lock);
    if (component != "") {
        m_currentMessage = prefix + component + ": " + msg;
    } else {
        m_currentMessage = prefix + msg;
    }
    m_messageDispatcher.emit();
}

void DebuggerWindow::onPortFailed(Manager* mgr, const std::string& msg) {
    std::lock_guard<std::mutex> lck(m_lock);
    (void) mgr;
    m_currentMessage = msg;
    m_portDownDispatcher.emit();
}

DebuggerWindow::DebuggerCallbacks::DebuggerCallbacks(DebuggerWindow* window) :
            m_window(window)
{
}

void DebuggerWindow::DebuggerCallbacks::onRead(const std::unordered_set<BoundVariable>& vars) {
    std::lock_guard<std::mutex> lck(m_window->m_lock);
    m_window->m_pendingReadVars.insert(vars.begin(), vars.end());
    m_window->m_readVarDispatcher.emit();
}

void DebuggerWindow::DebuggerCallbacks::onWrite(const std::unordered_set<BoundVariable>& vars) {
    (void) vars;
}

bool DebuggerWindow::DebuggerCallbacks::getSoftKill() {
    std::lock_guard<std::mutex> lck(m_window->m_lock);
    return m_window->m_softKill;
}

void DebuggerWindow::DebuggerCallbacks::postHardKill(bool hardKillValid, bool hardKill) {
    std::lock_guard<std::mutex> lck(m_window->m_lock);
    m_window->m_hardKill = hardKill;
    m_window->m_hardKillValid = hardKillValid;
    m_window->m_killDispatcher.emit();
}

void DebuggerWindow::DebuggerCallbacks::onDisconnect() {
    std::lock_guard<std::mutex> lck(m_window->m_lock);
    m_window->m_devInfo = nullptr;
    m_window->m_connectDispatcher.emit();
}
