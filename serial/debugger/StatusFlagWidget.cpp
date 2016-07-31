#include "StatusFlagWidget.h"

StatusFlagWidget::StatusFlagWidget(const Glib::ustring& goodText, const Glib::ustring& badText, const Glib::ustring& inactiveText) :
    m_state(true),
    m_active(true),
    m_goodText(goodText),
    m_badText(badText),
    m_inactiveText(inactiveText)
{
    m_label.set_margin_left(5);
    m_label.set_margin_right(5);

    set_vexpand();
    set_valign(Gtk::ALIGN_FILL);
    add(m_label);

    set_state(false);
}

StatusFlagWidget::~StatusFlagWidget() {
}

void StatusFlagWidget::set_state(bool state) {
    if (m_state != state && m_active) {
        if (state) {
            m_label.set_text(m_goodText);
            override_background_color(Gdk::RGBA("#A1B56C"));
        } else {
            m_label.set_text(m_badText);
            override_background_color(Gdk::RGBA("#AB4642"));
        }
    }
    m_state = state;
}

void StatusFlagWidget::set_active(bool active) {
    if (m_active != active) {
        m_active = active;
        if (active) {
            m_state = !m_state;
            set_state(!m_state);
        } else {
            m_label.set_text(m_inactiveText);
            override_background_color(Gdk::RGBA("#B8B8B8"));
        }
    }
}
