#include "temperature.h"
#include "libshm/c/shm.h"

using namespace std;

/* Temperature Windows */
/*  maybe another time...
WINDOW *cpu_info;
WINDOW *thruster_info;
WINDOW *board_info;
WINDOW *other_info;
WINDOW *weather_forecast;
*/
WINDOW *temperature_info;


/* Thruster Temperature Vars 
ShmSharedVar<double> port_thrust_temp("/diagnostics/motors/temperature/port");
ShmSharedVar<double> stbd_thrust_temp("/diagnostics/motors/temperature/starboard");
ShmSharedVar<double> fore_thrust_temp("/diagnostics/motors/temperature/fore");
ShmSharedVar<double> aft_thrust_temp("/diagnostics/motors/temperature/aft"); */

/* Circuit Board Temperature Vars 
ShmSharedVar<int> status_board_temp("/sensors/status/temperature");
ShmSharedVar<double> tcm2_temp("/sensors/tcm2/temperature");
ShmSharedVar<double> distro_board_temp("/diagnostics/power/distro/aft_temp");*/

/* Other Temperature Info 
ShmSharedVar<double> port_batt_temp("/diagnostics/power/pod/port/temp");
ShmSharedVar<double> stbd_batt_temp("/diagnostics/power/pod/starboard/temp");
*/

/* Updates the CPU page */
void update_temperature() {

    struct diagnostics_cpu diagnostics_cpu;
    shm_getg(diagnostics_cpu, diagnostics_cpu);

    wborder(temperature_info,0,0,0,0,0,0,0,0);

    mvwprintw(temperature_info,1,7,"Temperature Info");
    
    /* CPU Temps */
    
    mvwprintw(temperature_info,3,7,"CPU Info");
    
    //mvwprintw(temperature_info,5,4,"CPU Temp :  ");
    //wattrset(temperature_info,acomp(diagnostics_cpu.temperature,CPU_CPU_TEMP_WARN,CPU_CPU_TEMP_ERR));
    //wprintw(temperature_info,"%3.1f C",diagnostics_cpu.temperature);
    //wattrset(temperature_info,0);

    //mvwprintw(temperature_info,6,4,"Mobo Temp:  ");
    //wattrset(temperature_info,acomp(diagnostics_cpu.m_b_temp,CPU_MOBO_TEMP_WARN,CPU_MOBO_TEMP_ERR));
    //wprintw(temperature_info,"%3.1f C",diagnostics_cpu.m_b_temp);
    //wattrset(temperature_info,0);

    //mvwprintw(temperature_info,7,4,"Temp 3   :  ");
    //wattrset(temperature_info,acomp(diagnostics_cpu.temp3,CPU_TEMP3_WARN,CPU_TEMP3_ERR));
    //wprintw(temperature_info,"%3.1f C",diagnostics_cpu.temp3);
    //wattrset(temperature_info,0);

    /* Thruster Temps */
    
    //mvwprintw(temperature_info,9,7,"Thruster Info");
    /*
    mvwprintw(temperature_info,11,4,"CPU Temp :  ");
    wattrset(temperature_info,acomp(cpu_diagnostics_cpu.temperature,CPU_CPU_TEMP_WARN,CPU_CPU_TEMP_ERR));
    wprintw(temperature_info,"%3.1f C",cpu_diagnostics_cpu.temperature);
    wattrset(temperature_info,0);

    mvwprintw(temperature_info,12,4,"Mobo Temp:  ");
    wattrset(temperature_info,acomp(cpu_diagnostics_cpu.m_b_temp,CPU_MOBO_TEMP_WARN,CPU_MOBO_TEMP_ERR));
    wprintw(temperature_info,"%3.1f C",cpu_diagnostics_cpu.m_b_temp);
    wattrset(temperature_info,0);

    mvwprintw(temperature_info,13,4,"Temp 3   :  ");
    wattrset(temperature_info,acomp(cpu_diagnostics_cpu.temp3,CPU_TEMP3_WARN,CPU_TEMP3_ERR));
    wprintw(temperature_info,"%3.1f C",cpu_diagnostics_cpu.temp3);
    wattrset(temperature_info,0);
    
    mvwprintw(temperature_info,14,4,"Temp 3   :  ");
    wattrset(temperature_info,acomp(cpu_diagnostics_cpu.temp3,CPU_TEMP3_WARN,CPU_TEMP3_ERR));
    wprintw(temperature_info,"%3.1f C",cpu_diagnostics_cpu.temp3);
    wattrset(temperature_info,0);
    */    

    move(getmaxy(stdscr)-1,getmaxx(stdscr)-1);

}





