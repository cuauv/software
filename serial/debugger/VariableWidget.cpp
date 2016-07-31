#include "VariableWidget.h"

#include <gdk/gdkkeysyms.h>

#include <string>
#include <map>

using namespace cuauv::serial;

VariableWidget::VariableWidget(bool isWriteable) :
    m_isWriteable(isWriteable),
    m_headerNameLabel("Name"),
    m_headerTypeLabel("Type"),
    m_headerValueLabel("Value")
{
    m_grid.set_row_spacing(10);
    m_grid.set_column_spacing(10);

    m_frame.add(m_grid);
    set_shadow_type(Gtk::SHADOW_IN);

    add(m_frame);
}

void VariableWidget::setVars(std::shared_ptr<Manager> manager, const std::unordered_set<BoundVariable>& vars) {
    clear();

    // Emit header for grid
    m_grid.attach(m_headerNameLabel, 0, 0, 1, 1);
    m_grid.attach(m_headerTypeLabel, 1, 0, 1, 1);
    m_grid.attach(m_headerValueLabel, 2, 0, 1, 1);

    m_manager = manager;
    std::map<std::string, BoundVariable> sortedVars;
    for (auto var : vars) {
        sortedVars.insert(std::make_pair(var.name(), var));
    }

    int row_num = 1;
    for (auto var : sortedVars) {
        // NOTE: piecewise_construct allows splitting the emplace into two parts
        // the first tuple is the arguments to the key constructor
        // the second tuple is the arguments to the value constructor
        m_varMap.emplace(std::piecewise_construct,
                         std::forward_as_tuple(var.second),
                         std::forward_as_tuple(this, row_num, var.second));
        row_num++;
    }

    m_grid.show_all();
}

void VariableWidget::clear() {
    m_varMap.clear(); // destroying the VariableRow also removes the widgets from the grid
    m_grid.remove_row(0); // clear the header row too
    
    m_manager = nullptr;
}

void VariableWidget::updateVars(const std::unordered_set<BoundVariable>& vars) {
    for (auto var : vars) {
        auto it = m_varMap.find(var);
        if (it != m_varMap.end()) {
            it->second.update(var);
        }
    }
}

VariableWidget::VariableRow::VariableRow(VariableWidget* widget, int grid_row, const BoundVariable& var) :
    m_widget(widget),
    m_var(var),
    m_nameLabel(var.name()),
    m_typeLabel(typeToString(var.type())),
    m_readButton("Update Now")
{
    if (m_widget->m_isWriteable) {
        m_valueEntry.set_editable(true);
        m_valueEntry.signal_activate().connect(sigc::mem_fun(*this, &VariableWidget::VariableRow::readInput));
        m_valueEntry.signal_focus_out_event().connect(sigc::mem_fun(*this, &VariableWidget::VariableRow::on_focus_out));
        m_valueEntry.signal_key_press_event().connect(sigc::mem_fun(*this, &VariableWidget::VariableRow::on_key_press));
    } else {
        m_valueEntry.set_editable(false);
        m_valueEntry.set_sensitive(false);
    }
    reset_entry();

    m_widget->m_grid.attach(m_nameLabel, 0, grid_row, 1, 1);
    m_widget->m_grid.attach(m_typeLabel, 1, grid_row, 1, 1);
    m_widget->m_grid.attach(m_valueEntry, 2, grid_row, 1, 1);

    if (!m_widget->m_isWriteable) {
        m_readButton.signal_clicked().connect(sigc::mem_fun(*this, &VariableRow::on_device_read));
        m_widget->m_grid.attach(m_readButton, 3, grid_row, 1, 1);
    }
}

void VariableWidget::VariableRow::update(const BoundVariable& var) {
    m_var = var;

    reset_entry();
}

void VariableWidget::VariableRow::readInput() {
    auto text = m_valueEntry.get_text();
    if (m_var.type() == Variable::Type::FLOAT) {
        try {
            float value = std::stof(text);
            m_var = m_var.bind(value);
            m_widget->m_manager->submitWrite({m_var});
        } catch (std::exception& e) {
            // ignore, old value will be restored
        }
    } else {
        try {
            int value = std::stoi(text);
            m_var = m_var.bind(value);
            m_widget->m_manager->submitWrite({m_var});
        } catch (std::exception& e) {
            // ignore, old value will be restored
        }
    }

    reset_entry();
}

void VariableWidget::VariableRow::on_device_read() {
    m_widget->m_manager->submitRead({m_var});
}

void VariableWidget::VariableRow::reset_entry() {
    if (m_var.type() == Variable::Type::FLOAT) {
        m_valueEntry.set_text(std::to_string(m_var.getFloat()));
    } else {
        m_valueEntry.set_text(std::to_string(m_var.getInt()));
    }
}

bool VariableWidget::VariableRow::on_focus_out(GdkEventFocus* event) {
    (void) event;
    reset_entry();
    return false;
}

bool VariableWidget::VariableRow::on_key_press(GdkEventKey* event) {
    if (event->keyval == GDK_KEY_Escape) {
        reset_entry();
        return true;
    }
    return false;
}

Glib::ustring VariableWidget::VariableRow::typeToString(Variable::Type type) {
    switch (type) {
        case Variable::Type::UINT8:
            return "uint8";
        case Variable::Type::INT8:
            return "int8";
        case Variable::Type::UINT16:
            return "uint16";
        case Variable::Type::INT16:
            return "int16";
        case Variable::Type::FLOAT:
            return "float";
        default:
            return "unknown";
    }
}
