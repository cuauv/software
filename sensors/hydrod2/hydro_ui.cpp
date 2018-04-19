#include <algorithm>
#include <curses.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sstream>
#include "libshm/c/shm.h"

//using namespace AUV;
using namespace std;

WINDOW *phase_hydro;
WINDOW *search_params;
WINDOW *hydro_parameters;
WINDOW *hydro_flags;
WINDOW *hydro_data;
WINDOW *hydro_search;

#define TRACK_MODE 0
#define SEARCH_MODE 1

struct hydrophones_settings vars_settings;
struct hydrophones_results vars_results;

#define SPEC_WIDTH 45 
#define SPEC_DISPLAY 14
#define DATA_DISPLAY 12
double headings[DATA_DISPLAY];
double elevations[DATA_DISPLAY];
int intervals[DATA_DISPLAY];
int intensities[DATA_DISPLAY];
int counts[DATA_DISPLAY];
int ringbuf_indx;

char specgram[SPEC_DISPLAY][SPEC_WIDTH];
int specgram_indx;
int lastID;

int input_mode;
int input_field;
string input_buffer;

#define FIELD_FREQUENCY 1
#define FIELD_GAIN      2
#define FIELD_DEAD      3
#define FIELD_MAG       4
#define FIELD_ANG       5

const int ATTR_EDIT = COLOR_PAIR(1) | A_BOLD;


/* return a binary string representing 32 bit integer */
string dec2bin(int n){
    const int size=sizeof(n)*8;
    std::string res;
    bool s=0;
    for (int a=0;a<size;a++){
        bool bit=n>>(size-1);
        if (bit)
            s=1;
        if (s)
            res.push_back(bit+'0');
        n<<=1;
    }
    if (!res.size())
        res.push_back('0');
    return res;
}

void update_phase_hydro() {
    werase(phase_hydro);
    wborder(phase_hydro,0,0,0,0,0,0,0,0);
    mvwprintw(phase_hydro,1,3,"Hydrophone Phase");
    /* Phase X */
    mvwprintw(phase_hydro,2,2,"Phase X:  ");
    wprintw(phase_hydro,"%1.7f",vars_results.phaseX);
    
    /* Phase Y */
    mvwprintw(phase_hydro,3,2,"Phase Y:  ");
    wprintw(phase_hydro,"%1.7f",vars_results.phaseY);
    
	/* Phase Z */
    mvwprintw(phase_hydro,4,2,"Phase Z:  ");
    wprintw(phase_hydro,"%1.7f",vars_results.phaseZ);

	mvwprintw(phase_hydro,5,2,"Mode:     ");
	wprintw(phase_hydro,"%9i",vars_settings.dsp_mode);
    
    move(getmaxy(stdscr)-1,getmaxx(stdscr)-1);
}

void update_search_params() {
    werase(search_params);
	wborder(search_params,0,0,0,0,0,0,0,0);
    mvwprintw(search_params,1,3,"Search Parameters");

    mvwprintw(search_params,2,2,"Thresh: ");
    wprintw(search_params,"%10.2f", vars_settings.searchThresh);

    mvwprintw(search_params,3,2,"Center: ");
    wprintw(search_params,"%10d", vars_settings.searchCenter);
    
    mvwprintw(search_params,4,2,"Delta : ");
    wprintw(search_params,"%10d", vars_settings.searchDelta);
    
    mvwprintw(search_params,5,2,"Step  : ");
    wprintw(search_params,"%10d", vars_settings.searchStep);

	mvwprintw(search_params,6,2,"Length: ");
	wprintw(search_params,"%10d", vars_settings.searchLength);
} 

void update_hydro_flags() {
    werase(hydro_flags);
	wborder(hydro_flags,0,0,0,0,0,0,0,0);
	wattroff(hydro_flags, A_STANDOUT);
	if (vars_settings.dsp_mode) {
		wattron(hydro_flags, A_STANDOUT);
		mvwprintw(hydro_flags, 1, 47, "SRC");
		wattroff(hydro_flags, A_STANDOUT);
	} else {
		mvwprintw(hydro_flags, 1, 47, "TRK");
	}
    
	
}

//colors on, also returns the field value to be used based on the state
double won(int field, double fallback){
    if(input_mode && input_field == field){
        wattron(hydro_parameters, ATTR_EDIT);
        fallback = atof(input_buffer.c_str());
    }
    return fallback;
}

//colors off
void woff(int field){
    if(input_mode && input_field == field)
        wattroff(hydro_parameters, ATTR_EDIT);
}


void update_track_parameters() {
    werase(hydro_parameters);
	wborder(hydro_parameters,0,0,0,0,0,0,0,0);
    mvwprintw(hydro_parameters,1,4,"Track Parameters");

    double hf = won(FIELD_FREQUENCY, vars_settings.trackFrequency);
    mvwprintw(hydro_parameters,3,2,"Freq: ");
    wprintw(hydro_parameters,"%10.2f Hz", hf);
    woff(FIELD_FREQUENCY);
    
    int hd = won(FIELD_DEAD, vars_settings.trackDeadtime);
    mvwprintw(hydro_parameters,4,2,"DeadTime: ");
    wprintw(hydro_parameters,"%9i", hd);
    woff(FIELD_DEAD);
    
    double ha = won(FIELD_ANG, vars_settings.trackAngleThresh);
    mvwprintw(hydro_parameters,5,2,"ThrshAng: ");
    wprintw(hydro_parameters,"%9.2f", ha);
    woff(FIELD_ANG);
    
    double hm = won(FIELD_MAG, vars_settings.trackMagThresh);
    mvwprintw(hydro_parameters,6,2,"ThrshMag: ");
    wprintw(hydro_parameters,"%9.0f",hm);
    woff(FIELD_MAG);
   
    int hg = won(FIELD_GAIN, vars_settings.gain);
	mvwprintw(hydro_parameters,7,2,"Gain:     ");
    wprintw(hydro_parameters,"%9i", hg);
    woff(FIELD_GAIN);
}

void zero_buffers() {
	memset(headings, 0, sizeof(double) * DATA_DISPLAY);
	memset(elevations, 0, sizeof(double) * DATA_DISPLAY);
	memset(intervals, 0, sizeof(int) * DATA_DISPLAY);
	memset(intensities, 0, sizeof(int) * DATA_DISPLAY);
	memset(counts, 0, sizeof(int) * DATA_DISPLAY);
	ringbuf_indx = 0;
    
    memset(specgram, 0, sizeof(char) * DATA_DISPLAY);
    specgram_indx = 0;
} 

#define TICKSPACE 6
void update_hydro_search() {
    string bins;
	int i, j, maxbin, indx;

    // convert int to binary string
    bins = dec2bin(vars_results.search_bins);
   
    // pad the string appropriately
    while(bins.length() < 32){
        bins.insert(0, "0");
    }
    // reverse it
    reverse(bins.begin(), bins.end());
    // pad some more so that 1->16000
    bins.insert(0,"00");
    bins.insert(bins.length()-1, "0000");

    werase(hydro_search);
	wborder(hydro_search,0,0,0,0,0,0,0,0);
    mvwprintw(hydro_search,1,11,"Hydrophone Data-- Search Mode");

    //label the frequency bins
    for (i=8; i<41; i+=TICKSPACE) {
        mvwprintw(hydro_search, 3, i-2, "%5.2f", (((double)(i-6)) * vars_settings.searchStep + (vars_settings.searchCenter - vars_settings.searchDelta) ) / 1000.0);

        mvwprintw(hydro_search, 4, i, "|");
    }

    //draw the line
    for (i=4; i<41; i++) {
        mvwprintw(hydro_search, 5,i, "-");
    }
        
    mvwprintw(hydro_search, 5, 3, "/");

    //now the side, w/ second labels
    for (i=6; i<20; i++) {
        mvwprintw(hydro_search, i, 3, "|");
    }

    mvwprintw(hydro_search,  7, 2, "1");
    mvwprintw(hydro_search, 12, 2, "5");
    mvwprintw(hydro_search, 17, 1, "10");
    mvwprintw(hydro_search, 22, 1, "15");

    //save the data for later
    maxbin = min(SPEC_WIDTH, (int)(bins.length())); 

    if (vars_results.search_count != lastID) {
        lastID = vars_results.search_count;

        specgram_indx++;
        specgram_indx %= SPEC_DISPLAY;
        for (j=0; j<maxbin; j++) {
            specgram[specgram_indx][j] = (bins[j] == '1') ? 'x' : '.';
        }
     }

     //now, print the actual data
    for (i=0; i < SPEC_DISPLAY; i++) {
	    indx = (specgram_indx-i+SPEC_DISPLAY) % SPEC_DISPLAY;

        for (j=0; j<maxbin; j++) {
	        mvwprintw(hydro_search, i+6,j+4, "%c", specgram[indx][j]);
            
        }
    }
}

void update_hydro_data() {
	int i, indx;
	
    // track mode
	if (vars_results.ping_count != counts[ringbuf_indx]) {
	    ringbuf_indx++;
	    ringbuf_indx %= DATA_DISPLAY;
	    headings[ringbuf_indx] = vars_results.heading;
	    elevations[ringbuf_indx] = vars_results.elevation;
	    intervals[ringbuf_indx] = vars_results.ping_time;
	    intensities[ringbuf_indx] = vars_results.intensity;
        counts[ringbuf_indx] = vars_results.ping_count;
    }
	
    werase(hydro_data);
    wborder(hydro_data,0,0,0,0,0,0,0,0);
    mvwprintw(hydro_data,1,11,"Hydrophone Data-- Track Mode");

    mvwprintw(hydro_data, 3,3, "HEADING");
    mvwprintw(hydro_data, 4,3, "  DEG  ");

    mvwprintw(hydro_data, 3,15, "ELEVATN");
    mvwprintw(hydro_data, 4,15, "  DEG  ");
	
    mvwprintw(hydro_data, 3,28, "INTRVAL");
    mvwprintw(hydro_data, 4,28, "   ms  ");
	
    mvwprintw(hydro_data, 3,40, "INTNSITY");
	
    for (i=0; i < DATA_DISPLAY; i++) {
	    indx = (ringbuf_indx-i+DATA_DISPLAY) % DATA_DISPLAY;
	    mvwprintw(hydro_data, i+6,3, " %5.2f ", headings[indx]);
	    mvwprintw(hydro_data, i+6,15, " %5.2f ", elevations[indx]);
	    mvwprintw(hydro_data, i+6,28, " %5i ", intervals[indx]);
	    mvwprintw(hydro_data, i+6,40, "%8i", intensities[indx]);
    }
}

/* Cleanup Function - any cleanup code goes here */
void shutdown_display(int signal) {
    endwin();
}

void init_display() {
	/* Signal Handler */
    signal(SIGTERM, shutdown_display);

    /* Set up curses/color */
    initscr();
    noecho();
    nodelay(stdscr,TRUE);
    keypad(stdscr,TRUE);
   
    start_color();  

    use_default_colors();

    init_pair(1, COLOR_RED, -1);

	zero_buffers();
    lastID = 255;
	
	phase_hydro = newwin(7,23,17,0);
	search_params = newwin(8,23,9,0);
	hydro_flags = newwin(3, 51, 0, 24);
	hydro_parameters = newwin(9, 23, 0, 0);
	hydro_data = newwin(21, 51, 3, 24);
	hydro_search = newwin(21, 51, 3, 24);

}

/*Process buffer and exit input mode*/
void setField(){
    const char * input_c = input_buffer.c_str();

    switch(input_field){
        case FIELD_FREQUENCY:
            vars_settings.trackFrequency = atof(input_c);
            break;
        case FIELD_GAIN:
            vars_settings.gain = atoi(input_c);
            break;
        case FIELD_DEAD:
            vars_settings.trackDeadtime = atoi(input_c);
            break;
        case FIELD_ANG:
            vars_settings.trackAngleThresh = atof(input_c);
            break;
        case FIELD_MAG:
            vars_settings.trackMagThresh = atoi(input_c);
            break;
    }

    shm_setg(hydrophones_settings, vars_settings);

    input_mode = 0;
    input_field = 0;
    input_buffer.clear();
}


/* Handles keyboard input */
void handleInput() {
    int x;
    
    //get the current character and handle it
    nodelay(stdscr, TRUE); // don't block on keyreads
    cbreak();
    noecho();
    keypad(stdscr, TRUE); // let us read keys other than alpha nums

    x = getch();
    if(input_mode){ //key combinations to accept in input mode
        char concat = 0;
        switch(x){
            case ERR:
                break;
            case '1':
	        case '2':
	        case '3':
	        case '4':
	        case '5':
	        case '6':
	        case '7':
	        case '8':
	        case '9':
	        case '0':
            case '.':
                concat = x;
                break;
            case KEY_BACKSPACE:
                input_buffer = input_buffer.substr(0, input_buffer.length() - 1);
                break;
            case KEY_ENTER:
			case '\n':
			case '\r':	
                setField();
                break;
            case 27: //escape key
                input_mode = 0;
                input_buffer.clear();
                break;
        }

        if(concat){
        //concatinate input to buffer string
            stringstream ss;
            string s;
            ss << concat;
            ss >> s;
            input_buffer = input_buffer + s;
        
        }

    }else{ //key combinations to accept in command mode
        switch(x){
            case ERR:
                // no key pressed. Do nothing
                break;
            case 115: // 's' key
                vars_settings.dsp_mode = 1;
                shm_setg(hydrophones_settings, vars_settings);
                break;
            case 116: // 't' key
                vars_settings.dsp_mode = 0;
                shm_setg(hydrophones_settings, vars_settings);
                break;

            //FIELD MODIFICATIONS
            case 102: // 'f' key
                input_mode = 1;
                input_field = FIELD_FREQUENCY;
                break;
            case 103: // 'g' key
                input_mode = 1;
                input_field = FIELD_GAIN;
                break;
            case 100: // 'd'
                input_mode = 1;
                input_field = FIELD_DEAD;
                break;
            case 109: // 'm'
                input_mode = 1;
                input_field = FIELD_MAG;
                break;
            case 97: // 'a'
                input_mode = 1;
                input_field = FIELD_ANG;
                break;
        }
    }



 	flushinp();// trying to prevent delayed input from filled buffer
}


int main() {

    input_mode = 0;
    input_field = 0;

    shm_init();

	init_display();


	for (;;) {
        shm_getg(hydrophones_settings, vars_settings);
        shm_getg(hydrophones_results, vars_results);

		update_phase_hydro();
		update_search_params();
		update_hydro_flags();
		update_track_parameters();
        if (vars_settings.dsp_mode == SEARCH_MODE) {
		    update_hydro_search();
        } else {
		    update_hydro_data();
        }
        // Use wnoutrefresh to batch all refreshes together
		wnoutrefresh(phase_hydro);
		wnoutrefresh(search_params);
		wnoutrefresh(hydro_flags);
		wnoutrefresh(hydro_parameters);
        if (vars_settings.dsp_mode == SEARCH_MODE) {
            wnoutrefresh(hydro_search);
        } else {
		    wnoutrefresh(hydro_data);
        }

        // Draw the refreshes from above
        doupdate();
        
        handleInput();
        
        usleep(70000);
    }
	shutdown_display(0);
	return 0;
}




