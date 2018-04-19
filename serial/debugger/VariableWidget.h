#pragma once

#include <gtkmm/scrolledwindow.h>
#include <gtkmm/frame.h>
#include <gtkmm/grid.h>
#include <gtkmm/label.h>
#include <gtkmm/entry.h>
#include <gtkmm/button.h>

#include <Manager.h>
#include <Variable.h>

#include <unordered_map>

class VariableWidget : public Gtk::ScrolledWindow {
    public:
        // Creates a read-only VariableWidget
        VariableWidget(bool isWriteable = false);

        void setVars(std::shared_ptr<cuauv::serial::Manager> manager, const std::unordered_set<cuauv::serial::BoundVariable>& vars);
        void clear();

        void updateVars(const std::unordered_set<cuauv::serial::BoundVariable>& vars);
    private:
        class VariableRow {
            public:
                VariableRow(VariableWidget* widget, int grid_row, const cuauv::serial::BoundVariable& var);

                void update(const cuauv::serial::BoundVariable& var);
                
            protected:
                void readInput();
                void on_device_read();
                void reset_entry();

                bool on_focus_out(GdkEventFocus* event);
                bool on_key_press(GdkEventKey* event);
            private:
                Glib::ustring typeToString(cuauv::serial::Variable::Type type);

                VariableWidget* m_widget;

                cuauv::serial::BoundVariable m_var;
                Gtk::Label m_nameLabel;
                Gtk::Label m_typeLabel;
                Gtk::Entry m_valueEntry;
                Gtk::Button m_readButton;
        };

        const bool m_isWriteable;

        Gtk::Grid m_grid;
        Gtk::Frame m_frame;

        std::shared_ptr<cuauv::serial::Manager> m_manager;

        std::unordered_map<cuauv::serial::Variable, VariableRow> m_varMap;

        Gtk::Label m_headerNameLabel;
        Gtk::Label m_headerTypeLabel;
        Gtk::Label m_headerValueLabel;
};
