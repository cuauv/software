#include "libshm/c/shm.h"
#include "power.h"

using namespace std;

/* Battery Windows */
WINDOW *main_batt_status;
WINDOW *main_distro_status;
WINDOW *batt_port;
WINDOW *batt_starboard;
/* Distro Windows */
WINDOW *sensorpwr_5v;
WINDOW *sensorpwr_12v;

/* Battery Status Functions - for coloring items in main window */
int port_batt_status() {
    int s = ATTR_OK;
    int r;
    
    struct merge_status merge_status;
    shm_getg(merge_status, merge_status);

    /* Current */
    r = acomp(merge_status.current_port ,BATT_CURR_WARN,BATT_CURR_ERR);
    if (r == (ATTR_ERR)) return ATTR_ERR;
    if (r == (ATTR_WARN)) s = ATTR_WARN;

    /* Voltage */
    r = dcomp(merge_status.voltage_port ,BATT_VOLT_WARN,BATT_VOLT_ERR);
    if (r == (ATTR_ERR)) return ATTR_ERR;
    if (r == (ATTR_WARN)) s = ATTR_WARN;

    /* Temperature */
    //r = acomp(merge_status.temp_port ,BATT_TEMP_WARN,BATT_TEMP_ERR);
    //if (r == (ATTR_ERR)) return ATTR_ERR;
    //if (r == (ATTR_WARN)) s = ATTR_WARN;

    return s;
}

int starboard_batt_status() {
    int s = ATTR_OK;
    int r;

    struct merge_status merge_status;
    shm_getg(merge_status, merge_status);

    /* Current */
    r = acomp(merge_status.current_starboard ,BATT_CURR_WARN,BATT_CURR_ERR);
    if (r == (ATTR_ERR)) return ATTR_ERR;
    if (r == (ATTR_WARN)) s = ATTR_WARN;

    /* Voltage */
    r = dcomp(merge_status.voltage_starboard ,BATT_VOLT_WARN,BATT_VOLT_ERR);
    if (r == (ATTR_ERR)) return ATTR_ERR;
    if (r == (ATTR_WARN)) s = ATTR_WARN;

    /* Temperature */
    //r = acomp(merge_status.temp_starboard ,BATT_TEMP_WARN,BATT_TEMP_ERR);
    //if (r == (ATTR_ERR)) return ATTR_ERR;
    //if (r == (ATTR_WARN)) s = ATTR_WARN;
    
    return s;
}

/* Battery presence functions */
bool port_batt_present() {
    float voltage;
    shm_get(merge_status, voltage_port, voltage);
    return voltage > BATT_VOLT_PRESENT;
}

bool starboard_batt_present() {
    float voltage;
    shm_get(merge_status, voltage_starboard, voltage);
    return voltage > BATT_VOLT_PRESENT;
}

/* Distro Status Functions */
int distro_5v_status() {
    int s = ATTR_OK;
    int r;

    struct power_distribution_status pds;
    shm_getg(power_distribution_status, pds);
    
    /* Voltage */
    r = rcomp(pds.voltage_5v ,D5V_VOLT_WARN_LOW,D5V_VOLT_ERR_LOW,D5V_VOLT_WARN_HI,D5V_VOLT_ERR_HI);
    if (r == (ATTR_ERR)) return ATTR_ERR;
    if (r == (ATTR_WARN)) s = ATTR_WARN;

    /* Current */
    r = acomp(pds.current_5v ,D5V_CURR_WARN,D5V_CURR_ERR);
    if (r == (ATTR_ERR)) return ATTR_ERR;
    if (r == (ATTR_WARN)) s = ATTR_WARN;

    return s;
}

int distro_12v_status() {
    int s = ATTR_OK;
    int r;

    struct power_distribution_status pds;
    shm_getg(power_distribution_status, pds);
    
    /* Voltage */
    r = rcomp(pds.voltage_12v, D12V_VOLT_WARN_LOW,D12V_VOLT_ERR_LOW,D12V_VOLT_WARN_HI,D12V_VOLT_ERR_HI);
    if (r == (ATTR_ERR)) return ATTR_ERR;
    if (r == (ATTR_WARN)) s = ATTR_WARN;

    /* Current */
    r = acomp(pds.current_12v, D12V_CURR_WARN,D12V_CURR_ERR);
    if (r == (ATTR_ERR)) return ATTR_ERR;
    if (r == (ATTR_WARN)) s = ATTR_WARN;
    
    /* Faulted */
   // if (distro_12v_faulted.value()) return ATTR_ERR;

    return s;
}


/* Main page distro window */
void update_main_distro() {
    int s;

    wborder(main_distro_status,0,0,0,0,0,0,0,0);

    /* Title */
    mvwprintw(main_distro_status,1,2,"Sensor Power");

    /* 5V */
    mvwprintw(main_distro_status,3,2,"+5V  :  ");
    s = distro_5v_status();
    wattrset(main_distro_status,s);
    if (s == (ATTR_OK)) wprintw(main_distro_status," OK ");
    else if (s == (ATTR_WARN)) wprintw(main_distro_status,"WARN");
    else if (s == (ATTR_ERR)) wprintw(main_distro_status," ERR");
    wattrset(main_distro_status,0);
    /* 12V */
    mvwprintw(main_distro_status,4,2,"+12V :  ");
    s = distro_12v_status();
    wattrset(main_distro_status,s);
    if (s == (ATTR_OK)) wprintw(main_distro_status," OK ");
    else if (s == (ATTR_WARN)) wprintw(main_distro_status,"WARN");
    else if (s == (ATTR_ERR)) wprintw(main_distro_status," ERR");
    wattrset(main_distro_status,0);
}


/* Main page battery window*/
void update_main_batt() {
    int s;

    struct merge_status merge_status;
    shm_getg(merge_status, merge_status);

    wborder(main_batt_status,0,0,0,0,0,0,0,0);

    mvwprintw(main_batt_status,1,12,"Battery Info");
    
    /* Port */
    mvwprintw(main_batt_status,3,7,"Port");
    if (port_batt_present()) {
        // Port Percent
//        draw_progress_bar(main_batt_status,4, 2, 9, pod_port.percent , dcomp(pod_port.percent ,BATT_PERC_WARN,BATT_PERC_ERR));
 
        // Port Voltage
        mvwprintw(main_batt_status,5,2,"Voltage: ");
        wattrset(main_batt_status,dcomp(merge_status.voltage_port ,BATT_VOLT_WARN, BATT_VOLT_ERR));
        wprintw(main_batt_status,"%4.2f",merge_status.voltage_port );
        wattrset(main_batt_status,0);

        /* Port Status */
        mvwprintw(main_batt_status,6,2,"Status :  ");
        s = port_batt_status();
        wattrset(main_batt_status,s);
        if (s == (ATTR_OK)) wprintw(main_batt_status,"  OK");
        else if (s == (ATTR_WARN)) wprintw(main_batt_status,"WARN");
        else if (s == (ATTR_ERR)) wprintw(main_batt_status," ERR");
        wattrset(main_batt_status,0);
    }
    else mvwprintw(main_batt_status,5,3,"Not Present");

    // Starboard
    mvwprintw(main_batt_status,3,21,"Starboard");
    
    if (starboard_batt_present()) {
        // Starboard Percent
//        draw_progress_bar(main_batt_status,4, 19, 9, pod_starboard.percent ,dcomp(pod_starboard.percent ,BATT_PERC_WARN,BATT_PERC_ERR));

        // Starboard Voltage
        mvwprintw(main_batt_status,5,19,"Voltage: ");
        wattrset(main_batt_status,dcomp(merge_status.voltage_starboard ,BATT_VOLT_WARN, BATT_VOLT_ERR));
        wprintw(main_batt_status,"%4.2f",merge_status.voltage_starboard );
        wattrset(main_batt_status,0);
        
        /* Starboard Status */
        mvwprintw(main_batt_status,6,19,"Status :  ");
        s = starboard_batt_status();
        wattrset(main_batt_status,s);
        if (s == (ATTR_OK)) wprintw(main_batt_status,"  OK");
        else if (s == (ATTR_WARN)) wprintw(main_batt_status,"WARN");
        else if (s == (ATTR_ERR)) wprintw(main_batt_status," ERR");
        wattrset(main_batt_status,0);
     }
    else mvwprintw(main_batt_status,5,20,"Not Present");

    /* Time Remaining */
//    double time_remaining;
//    shm_get(pods, time_remaining, time_remaining);
//    int h = int(time_remaining); int m = (time_remaining - h) * 60;
//    mvwprintw(main_batt_status,8,8,"Est. Remaining: %d:%.2d",h,m);

    move(getmaxy(stdscr)-1,getmaxx(stdscr)-1);
}



/* Power Page Battery Windows */
void update_batt_port() {
    struct merge_status merge_status;
    shm_getg(merge_status, merge_status);

    bool present = port_batt_present();
    /* Needed to redraw window */
    werase(batt_port);

    wborder(batt_port,0,0,0,0,0,0,0,0);

    mvwprintw(batt_port,1,5,"Port Batt");

    if (present) {
//        /* Board ID */
//        mvwprintw(batt_port,3,2,"Board ID:   ");
//        wprintw(batt_port,"%2d",pod_port.board_id );
//        
//        /* Percent Fill */
//        mvwprintw(batt_port,5,2,"Fill:");
//        draw_progress_bar(batt_port,6,2,9,pod_port.percent ,dcomp(pod_port.percent ,BATT_PERC_WARN,BATT_PERC_ERR));

        /* Voltage */
        mvwprintw(batt_port,8,2,"Voltage: ");
        wattrset(batt_port,dcomp(merge_status.voltage_port ,BATT_VOLT_WARN, BATT_VOLT_ERR));
        wprintw(batt_port,"%4.2f",merge_status.voltage_port );
        wattrset(batt_port,0);

        /* Current */
        mvwprintw(batt_port,9,2,"Current:  ");
        wattrset(batt_port,acomp(merge_status.current_port ,BATT_CURR_WARN, BATT_CURR_ERR));
        wprintw(batt_port,"%4.2f",merge_status.current_port );
        wattrset(batt_port,0);

        /* Temp */
        //mvwprintw(batt_port,10,2,"Temp:   ");
        //wattrset(batt_port,acomp(merge_status.temp_port ,BATT_TEMP_WARN, BATT_TEMP_ERR));
        //wprintw(batt_port,"%3.1f C",merge_status.temp_port );
        //wattrset(batt_port,0);

//        /* Low Batt */
//        mvwprintw(batt_port,12,2,"Low Batt:   ");
//        wattrset(batt_port,(pod_port.low_batt ) ? ATTR_WARN : ATTR_OK);
//        wprintw(batt_port,(pod_port.low_batt ) ? "YES" : " NO");
//        wattrset(batt_port,0);
//
//        /* Discharge */
//        mvwprintw(batt_port,13,2,"Discharge:  ");
//        wattrset(batt_port,(pod_port.discharge_enabled ) ? ATTR_OK : ATTR_ERR);
//        wprintw(batt_port,(pod_port.discharge_enabled ) ? "YES" : " NO");
//        wattrset(batt_port,0);
//        /* charge */
//        mvwprintw(batt_port,14,1,"Chrg. Enbl.: ");
//        wattrset(batt_port,(pod_port.charge_detected ) ? ATTR_ERR : ATTR_OK);
//        wprintw(batt_port,(pod_port.charge_detected ) ? "YES" : " NO");
//        wattrset(batt_port,0);
    }
    else mvwprintw(batt_port,3,4, "Not Present");

    move(getmaxy(stdscr)-1,getmaxx(stdscr)-1);
}
    

void update_batt_starboard() {
    struct merge_status merge_status;
    shm_getg(merge_status, merge_status);

    bool present = starboard_batt_present();
    /* Needed to redraw window */
    werase(batt_starboard);

    wborder(batt_starboard,0,0,0,0,0,0,0,0);

    mvwprintw(batt_starboard,1,2,"Starboard Batt");

    if (present) {
//        /* Board ID */
//        mvwprintw(batt_starboard,3,2,"Board ID:   ");
//        wprintw(batt_starboard,"%2d",pod_starboard.board_id );
//        
//        /* Percent Fill */
//        mvwprintw(batt_starboard,5,2,"Fill:");
//        draw_progress_bar(batt_starboard,6,2,9,pod_starboard.percent ,dcomp(pod_starboard.percent ,BATT_PERC_WARN,BATT_PERC_ERR));

        /* Voltage */
        mvwprintw(batt_starboard,8,2,"Voltage: ");
        wattrset(batt_starboard,dcomp(merge_status.voltage_starboard ,BATT_VOLT_WARN, BATT_VOLT_ERR));
        wprintw(batt_starboard,"%4.2f",merge_status.voltage_starboard );
        wattrset(batt_starboard,0);

        /* Current */
        mvwprintw(batt_starboard,9,2,"Current:  ");
        wattrset(batt_starboard,acomp(merge_status.current_starboard ,BATT_CURR_WARN, BATT_CURR_ERR));
        wprintw(batt_starboard,"%4.2f",merge_status.current_starboard );
        wattrset(batt_starboard,0);

        /* Temp */
        //mvwprintw(batt_starboard,10,2,"Temp:   ");
        //wattrset(batt_starboard,acomp(pod.temp_starboard ,BATT_TEMP_WARN, BATT_TEMP_ERR));
        //wprintw(batt_starboard,"%3.1f C",pod.temp_starboard );
        //wattrset(batt_starboard,0);

//        /* Low Batt */
//        mvwprintw(batt_starboard,12,2,"Low Batt:   ");
//        wattrset(batt_starboard,(pod_starboard.low_batt ) ? ATTR_WARN : ATTR_OK);
//        wprintw(batt_starboard,(pod_starboard.low_batt ) ? "YES" : " NO");
//        wattrset(batt_starboard,0);
//
//        /* Discharge */
//        mvwprintw(batt_starboard,13,2,"Discharge:  ");
//        wattrset(batt_starboard,(pod_starboard.discharge_enabled ) ? ATTR_OK : ATTR_ERR);
//        wprintw(batt_starboard,(pod_starboard.discharge_enabled ) ? "YES" : " NO");
//        wattrset(batt_starboard,0);
//        /* charge */
//        mvwprintw(batt_starboard,14,1,"Chrg. Enbl.: ");
//        wattrset(batt_starboard,(pod_starboard.charge_detected ) ? ATTR_ERR : ATTR_OK);
//        wprintw(batt_starboard,(pod_starboard.charge_detected ) ? "YES" : " NO");
//        wattrset(batt_starboard,0);
    }
    else mvwprintw(batt_starboard,3,4, "Not Present");

    move(getmaxy(stdscr)-1,getmaxx(stdscr)-1);
}

/* Power Page Sensor Power Windows */

void update_distro_5v() {
    struct power_distribution_status pds;
    shm_getg(power_distribution_status, pds);

    wborder(sensorpwr_5v,0,0,0,0,0,0,0,0);
    mvwprintw(sensorpwr_5v,1,1,"5 Volt SensorPwr");
    /* Voltage */
    mvwprintw(sensorpwr_5v,3,2,"Voltage:  ");
    wattrset(sensorpwr_5v,rcomp(pds.voltage_5v, D5V_VOLT_WARN_LOW,D5V_VOLT_ERR_LOW,D5V_VOLT_WARN_HI,D5V_VOLT_ERR_HI));
    wprintw(sensorpwr_5v,"%4.2fV", pds.voltage_5v);
    wattrset(sensorpwr_5v,0);

    /* Current */
    mvwprintw(sensorpwr_5v,4,2,"Current:  ");
    wattrset(sensorpwr_5v,acomp(pds.current_5v, D5V_CURR_WARN,D5V_CURR_ERR));
    wprintw(sensorpwr_5v,"%4.2fA", pds.current_5v);
    wattrset(sensorpwr_5v,0);
    move(getmaxy(stdscr)-1,getmaxx(stdscr)-1);
}

void update_distro_12v() {
    struct power_distribution_status pds;
    shm_getg(power_distribution_status, pds);

    wborder(sensorpwr_12v,0,0,0,0,0,0,0,0);
    mvwprintw(sensorpwr_12v,1,1,"12 Volt SensorPwr");
    /* Voltage */
    mvwprintw(sensorpwr_12v,3,2,"Voltage:  ");
    wattrset(sensorpwr_12v,rcomp(pds.voltage_12v ,D12V_VOLT_WARN_LOW,D12V_VOLT_ERR_LOW,D12V_VOLT_WARN_HI,D12V_VOLT_ERR_HI));
    wprintw(sensorpwr_12v,"%4.2fV",pds.voltage_12v );
    wattrset(sensorpwr_12v,0);

    /* Current */
    mvwprintw(sensorpwr_12v,4,2,"Current:  ");
    wattrset(sensorpwr_12v,acomp(pds.current_12v ,D12V_CURR_WARN,D12V_CURR_ERR));
    wprintw(sensorpwr_12v,"%4.2fA",pds.current_12v );
    wattrset(sensorpwr_12v,0);
    move(getmaxy(stdscr)-1,getmaxx(stdscr)-1);
}
