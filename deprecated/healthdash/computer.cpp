#include "libshm/c/shm.h"
#include "computer.h"

using namespace std;

/* CPU Window */
WINDOW *cpu_info;

/* Updates the CPU page */
void update_cpu() {
    wborder(cpu_info,0,0,0,0,0,0,0,0);

    mvwprintw(cpu_info,1,7,"Computer Info");

    struct diagnostics_cpu diagnostics_cpu;
    shm_getg(diagnostics_cpu, diagnostics_cpu);

    /* Voltages */
    mvwprintw(cpu_info,3,6,"VCore:   ");
    wattrset(cpu_info,acomp(diagnostics_cpu.vcore ,CPU_VCORE_WARN,CPU_VCORE_ERR));
    wprintw(cpu_info,"%5.2fV",diagnostics_cpu.vcore);
    wattrset(cpu_info,0);
    
    mvwprintw(cpu_info,4,6,"+12V :   ");
    wattrset(cpu_info,rcomp(diagnostics_cpu.plus_12v ,CPU_12V_WARN_LO,CPU_12V_ERR_LO,CPU_12V_WARN_HI,CPU_12V_ERR_HI));
    wprintw(cpu_info,"%5.2fV",diagnostics_cpu.plus_12v);
    wattrset(cpu_info,0);
    
    mvwprintw(cpu_info,5,6,"+3.3V:   ");
    wattrset(cpu_info,rcomp(diagnostics_cpu.plus_3_3v ,CPU_33V_WARN_LO,CPU_33V_ERR_LO,CPU_33V_WARN_HI,CPU_33V_ERR_HI));
    wprintw(cpu_info,"%5.2fV",diagnostics_cpu.plus_3_3v);
    wattrset(cpu_info,0);
    
    mvwprintw(cpu_info,6,6,"+5V  :   ");
    wattrset(cpu_info,rcomp(diagnostics_cpu.plus_5v ,CPU_5V_WARN_LO,CPU_5V_ERR_LO,CPU_5V_WARN_HI,CPU_5V_ERR_HI));
    wprintw(cpu_info,"%5.2fV",diagnostics_cpu.plus_5v);
    wattrset(cpu_info,0);
    
    /* Temps */
    mvwprintw(cpu_info,8,4,"CPU Temp :  ");
    wattrset(cpu_info,acomp(diagnostics_cpu.temperature ,CPU_CPU_TEMP_WARN,CPU_CPU_TEMP_ERR));
    wprintw(cpu_info,"%3.1f C",diagnostics_cpu.temperature);
    wattrset(cpu_info,0);

    mvwprintw(cpu_info,9,4,"Mobo Temp:  ");
    wattrset(cpu_info,acomp(diagnostics_cpu.m_b_temp ,CPU_MOBO_TEMP_WARN,CPU_MOBO_TEMP_ERR));
    wprintw(cpu_info,"%3.1f C",diagnostics_cpu.m_b_temp);
    wattrset(cpu_info,0);

    mvwprintw(cpu_info,10,4,"Temp 3   :  ");
    wattrset(cpu_info,acomp(diagnostics_cpu.temp3 ,CPU_TEMP3_WARN,CPU_TEMP3_ERR));
    wprintw(cpu_info,"%3.1f C",diagnostics_cpu.temp3);
    wattrset(cpu_info,0);

    /* Fan Speed */
    mvwprintw(cpu_info,12,2,"CPU Fan Speed: ");
    wprintw(cpu_info,"%4d RPM",diagnostics_cpu.fan);
    
    move(getmaxy(stdscr)-1,getmaxx(stdscr)-1);

}





