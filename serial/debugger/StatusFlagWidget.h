#pragma once

#include <gtkmm/label.h>
#include <gtkmm/eventbox.h>

class StatusFlagWidget : public Gtk::EventBox {
    public:
        StatusFlagWidget(const Glib::ustring& goodText, const Glib::ustring& badText, const Glib::ustring& inactiveText = "");
        virtual ~StatusFlagWidget();

        void set_state(bool state);
        void set_active(bool active);

    private:
        Gtk::Label m_label;
        bool m_state;
        bool m_active;
        const Glib::ustring m_goodText;
        const Glib::ustring m_badText;
        const Glib::ustring m_inactiveText;
};
