/* Battery Percentage */
#define BATT_PERC_WARN 25
#define BATT_PERC_ERR 10
/* Battery Voltage */
#define BATT_VOLT_WARN 21.625
#define BATT_VOLT_ERR 20.89
#define BATT_VOLT_PRESENT 10.0
/* Battery Current */
#define BATT_CURR_WARN 10
#define BATT_CURR_ERR 15
/* Battery Temperature */
#define BATT_TEMP_WARN 47
#define BATT_TEMP_ERR 50

/* Distro 5 Volt Voltage */
#define D5V_VOLT_WARN_LOW 4.75
#define D5V_VOLT_ERR_LOW 4.5
#define D5V_VOLT_WARN_HI 5.25
#define D5V_VOLT_ERR_HI 5.5
/* Distro 5 Volt Current */
#define D5V_CURR_WARN 1.5
#define D5V_CURR_ERR 2.0

/* Distro 12 Volt Voltage */
#define D12V_VOLT_WARN_LOW 11.4
#define D12V_VOLT_ERR_LOW 10.8
#define D12V_VOLT_WARN_HI 12.6
#define D12V_VOLT_ERR_HI 13.2
/* Distro 12 Volt Current */
#define D12V_CURR_WARN 0.75
#define D12V_CURR_ERR 1.0

/* Distro Hydrophone Voltage */
#define DHYDRO_VOLT_WARN_LOW 4.75
#define DHYDRO_VOLT_ERR_LOW 4.5
#define DHYDRO_VOLT_WARN_HI 5.25
#define DHYDRO_VOLT_ERR_HI 5.5
/* Distro Hydrophone Current */
#define DHYDRO_CURR_WARN 1.25
#define DHYDRO_CURR_ERR 1.66

/* Distro Stack Voltage */
#define DSTACK_VOLT_WARN_LOW 4.75
#define DSTACK_VOLT_ERR_LOW 4.5
#define DSTACK_VOLT_WARN_HI 5.25
#define DSTACK_VOLT_ERR_HI 5.5
/* Distro Stack Current */
#define DSTACK_CURR_WARN 0.9
#define DSTACK_CURR_ERR 1.2

/* Thruster Voltage */
#define THRUST_VOLT_WARN_LOW 21.625
#define THRUST_VOLT_ERR_LOW 20.89
#define THRUST_VOLT_WARN_HI 30.0
#define THRUST_VOLT_ERR_HI 33.0
/* Thruster Current (Port and Starboard) */
#define THRUST_PS_CURR_WARN 4000.
#define THRUST_PS_CURR_ERR 6000.
/* Thruster Current (Fore and Aft) */
#define THRUST_FA_CURR_WARN 4000
#define THRUST_FA_CURR_ERR 6000
/* Thruster Temperature */
#define THRUST_TEMP_WARN 47
#define THRUST_TEMP_ERR 50

/* Onboard Computer */
#define CPU_VCORE_WARN 200 /* Don't care about these 2 FIXME: Find real values*/
#define CPU_VCORE_ERR 200 
#define CPU_12V_WARN_LO 10
#define CPU_12V_ERR_LO 8
#define CPU_12V_WARN_HI 14
#define CPU_12V_ERR_HI 16
#define CPU_5V_WARN_LO 4
#define CPU_5V_ERR_LO 3
#define CPU_5V_WARN_HI 6
#define CPU_5V_ERR_HI 7
#define CPU_33V_WARN_LO 1
#define CPU_33V_ERR_LO 0
#define CPU_33V_WARN_HI 4
#define CPU_33V_ERR_HI 5
#define CPU_CPU_TEMP_WARN 30
#define CPU_CPU_TEMP_ERR 35
#define CPU_MOBO_TEMP_WARN 60
#define CPU_MOBO_TEMP_ERR 65
#define CPU_TEMP3_WARN 60
#define CPU_TEMP3_ERR 65
#define CPU_FAN_ERR 1.58e11 /* Not Used, tips at speed of light - FIXME: Decide how to handle this*/
