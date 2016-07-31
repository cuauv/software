#pragma once

#include <gtkmm/button.h>
#include <gtkmm/window.h>
#include <gtkmm/box.h>
#include <gtkmm/grid.h>
#include <gtkmm/paned.h>
#include <gtkmm/label.h>
#include <gtkmm/togglebutton.h>
#include <gtkmm/comboboxtext.h>
#include <gtkmm/infobar.h>
#include <glibmm/dispatcher.h>

#include "StatusFlagWidget.h"
#include "VariableWidget.h"

#include <Manager.h>
#include <DeviceCallbacks.h>
#include <Variable.h>
#include <Log.h>

class DebuggerWindow : public Gtk::Window {
    public:
        DebuggerWindow();
        virtual ~DebuggerWindow();

        void kill_manager();

        bool set_port(const std::string& port);


    protected:
        // Signal handlers:
        void on_change_port();
        void on_select_activate();
        void on_select_modified();
        void on_scan_ports();

        void on_port_down();
        void on_show_message();

        void on_kill_changed();
        void on_softkill_changed();
        void on_reset_device();
        void on_connection_changed();
        void on_vars_read();

        bool on_select_key_down(GdkEventKey* event);

        void show_error(const Glib::ustring& error);
        void hide_error();
        void on_message_response(int response);

    private:
        class DebuggerCallbacks : public cuauv::serial::DeviceCallbacks {
            public:
                DebuggerCallbacks(DebuggerWindow* window);
                // DeviceCallbacks interface
                virtual void onRead(const std::unordered_set<cuauv::serial::BoundVariable>& vars) override;
                virtual void onWrite(const std::unordered_set<cuauv::serial::BoundVariable>& vars) override;
                virtual bool getSoftKill() override;
                virtual void postHardKill(bool hardKillValid, bool hardKill) override;
                virtual void onDisconnect() override;
            private:
                DebuggerWindow* const m_window;
        };

        std::shared_ptr<cuauv::serial::DeviceCallbacks> onConnected(cuauv::serial::Manager* mgr, 
                                std::shared_ptr<cuauv::serial::DeviceInfo> devInfo);

        void onPortFailed(cuauv::serial::Manager* mgr, const std::string& msg);

        void onLog(cuauv::serial::Log::Level level, const std::string& component, const std::string& msg);

        // Overall frame widgets:
        Gtk::Box m_outerBox;
        Gtk::Grid m_statusGrid;
        Gtk::Grid m_statusFlagGrid;

        // Status bar widgets
        // Device info
        Gtk::Label m_nameLabel;
        Gtk::Label m_typeLabel;

        // Device status
        StatusFlagWidget m_hardKillFlag;
        StatusFlagWidget m_connectedFlag;
        StatusFlagWidget m_portStatusFlag;

        // Debugger setup
        Gtk::Button m_resetButton;
        Gtk::ToggleButton m_softKillButton;
        Gtk::Button m_scanButton;
        Gtk::ComboBoxText m_portSelect;
        Gtk::Button m_connectButton;

        // Variable panes
        VariableWidget m_readVarWidget;
        VariableWidget m_writeVarWidget;

        // Callback dispatchers
        Glib::Dispatcher m_killDispatcher;
        Glib::Dispatcher m_connectDispatcher;
        Glib::Dispatcher m_portDownDispatcher;
        Glib::Dispatcher m_messageDispatcher;
        Glib::Dispatcher m_readVarDispatcher;

        // Info bar
        Gtk::Label m_messageLabel;
        Gtk::InfoBar m_messageBar;

        bool m_scanActive;

        // State variables for interaction between callbacks and GUI thread
        std::mutex m_lock;
        bool m_hardKill;
        bool m_hardKillValid;
        bool m_softKill;
        std::string m_currentMessage;
        std::shared_ptr<cuauv::serial::DeviceInfo> m_devInfo;
        std::shared_ptr<cuauv::serial::Manager> m_manager;
        std::unordered_set<cuauv::serial::BoundVariable> m_pendingReadVars;
};
