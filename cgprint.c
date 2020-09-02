// Companion script to fortplot.f90 to call gnuplot from fortran scripts

#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

// Author: Anjishnu Sarkar
// Version: 0.15

#if defined(_WIN32)
    #define PLATFORM_NAME "windows" // Windows
    #include <windows.h>
#else
    #define PLATFORM_NAME "linux" // linux
#endif

FILE *gp ;

// https://gcc.gnu.org/onlinedocs/gfortran/Interoperable-Subroutines-and-Functions.html

int c_check_dir_(char *path);

// Open a pipe to gnuplot
void c_open_gp_(){
    gp = popen("gnuplot -persist","w") ; 
}

void c_kill_gnuplot_(){

    int sleep ;

    if (PLATFORM_NAME == "windows"){
//         printf("Can't kill gnuplot on windows.\n") ;
    // https://www.quora.com/What-is-the-easiest-way-to-find-a-process-and-kill-it-in-C++-on-Windows
    // https://support.kaspersky.com/6325#block3
    // https://stackoverflow.com/questions/1262708/suppress-command-line-output
    // https://superuser.com/questions/232703/taskkill-exe-dont-throw-an-error-if-the-process-is-not-running
//         system("taskkill /IM gnuplot.exe /F");
//         system("start /wait taskkill /IM gnuplot.exe /F");
        system("taskkill /IM gnuplot.exe /F >nul 2>&1");
        system("taskkill /IM gnuplot_qt.exe /F >nul 2>&1");
    } else {
        system("pkill -f 'gnuplot -persist'") ;
        system("pkill -f 'gnuplot_qt'") ;
        system("pkill -f 'gnuplot_x11 -persist'") ;
    }
}

// Print to gnuplot
void c_print_(char *gnucmd, int *nwlnp, int *debugp){
  
    int i, sleep, newline, debug ;
    newline = *nwlnp ;
    debug = *debugp ;
    char *nwlfmt ;    

    // If newline is required at the end of the string.
    if (newline){
        nwlfmt = "\n" ;
    } else {
    // No newline is required at the end of the string.
        nwlfmt = "" ;
    }

    // If debug only print to terminal
    if ( debug ){   
        printf("%s%s", gnucmd, nwlfmt);
    } else {        
    // Otherwise print to gnuplot
        fprintf(gp,"%s%s", gnucmd, nwlfmt) ;
    }
    
    fflush(gp) ;

}

// Create directory
void c_mkdir_(char *dirname){

    int check ;
    char cmdbuf[500] ;

    // Check if directory exists or not
    if (c_check_dir_(dirname) == 0) {
//         printf("Directory exists at path '%s'\n", dirname);
        ; 
    } else {
//         printf("Directory does not exists at path '%s'\n", dirname);        
    
        if (PLATFORM_NAME == "windows"){
            snprintf(cmdbuf,sizeof(cmdbuf), "if not exist \"%s\" mkdir \"%s\"", 
                        dirname, dirname) ;
        } else {
            snprintf(cmdbuf,sizeof(cmdbuf), "mkdir -p \"%s\"", dirname) ;
        }

        check = system(cmdbuf) ;
        if (check) {
            printf("Unable to create directory '%s'.\n", dirname ) ;
            exit(check) ;
        } 
    }
}

// Get file extension
const char *get_ext(const char *filename) {
    const char *dot = strrchr(filename, '.');
    if(!dot || dot == filename) return "";
    return dot + 1;
}

// Delete files of a specific extension from a particular directory
int c_delete_ext_files_( char *dirname, char *ext ){

	struct dirent *de; // Pointer for directory entry 
    int res, status ;
    char path[1000]  ; 

    if (c_check_dir_(dirname) == 0){

    	// opendir() returns a pointer of DIR type. 
	    DIR *dr = opendir(dirname); 

        // Refer http://pubs.opengroup.org/onlinepubs/7990989775/xsh/readdir.html 
        // opendir returns NULL if couldn't open directory 
        // https://stackoverflow.com/questions/308695/how-do-i-concatenate-const-literal-strings-in-c
	    if (dr) { 
    	    while ((de = readdir(dr)) != NULL) {
                res = strcmp(get_ext(de->d_name),ext) ;
                if (res == 0) {

                    // Concatenate to get the path
                    strcat(strcat(strcpy(path,dirname),"/"),de->d_name) ;
//                     printf("Path = %s\n", path) ;
                    status = remove(path) ;

                    // Check return status of file removal
                    if (status != 0) {
                        printf("Unable to delete the file '%s/%s'.\n", 
                                dirname, de->d_name);
//                    } else {
//                        printf("Deleted the file '%s/%s'\n", 
//                              dirname, de->d_name) ; 
                    }
                }
            }
        } else {
		    printf("Could not open current directory: \"%s\"\n", dirname); 
		    return 1; 
        }
	    closedir(dr);	 
    }
    
    return 0 ;
}


// Remove an empty directory
void c_rmdir_(char *dirname){
    int status ;

    // Try to delete directory, only if it exists.
    if (c_check_dir_(dirname) == 0){
//     status = remove(dirname) ; // Works in linux
        status = rmdir(dirname) ; // Works in windows and linux
//     printf("'%s': '%d'\n", dirname, status) ;
        if ( status != 0 ) {
            printf("Couldn't remove the directory '%s'\n", dirname) ;
        }
    }
}

/** https://codeforwin.org/2018/03/c-program-check-file-or-directory-exists-not.html
 * Function to check whether a directory exists or not.
 * It returns 1 if given path is directory and  exists 
 * otherwise returns 0.
 */
int c_check_dir_(char *dirname){
    struct stat stats;

    stat(dirname, &stats);

    // Check for file existence
    if (S_ISDIR(stats.st_mode)){
        // Folder exists
        return 0;
    } else {
        // Folder does not exist
        return 1;
    }
}

// https://codeforwin.org/2018/03/c-program-check-file-or-directory-exists-not.html
int c_check_file_(char *filename){

    // Check for file existence
    if (access(filename, F_OK) == -1) {
        // File does not exist
        return 1;
    } else {
        // File exists
        return 0;
    }
}


void c_slumber_(float *secp) // cross-platform sleep function
{

    float seconds = *secp ;

//     printf("Delay in C = %f\n", seconds) ;

    #ifdef WIN32
//         printf("Using Sleep in Windows\n") ;
        Sleep(seconds * 1000); // Time in milliseconds
//     #elif _POSIX_C_SOURCE >= 199309L
//         printf("Using nanosleep\n") ;
//         struct timespec ts ;
//         ts.tv_sec = seconds ;
//         ts.tv_nsec = seconds * 1000000;
//         nanosleep(&ts, NULL);
    #else
//         printf("Using usleep in Linux\n") ;
        usleep(seconds * 1000 * 1000); // In microseconds
    #endif
}


void c_check_software_(char *program){

    int check ;
    char cmdbuf[500] ;

    if (PLATFORM_NAME == "windows"){
        snprintf(cmdbuf,sizeof(cmdbuf), "where \"%s\"", program) ;
        strcat(cmdbuf, " > nul") ;
    } else {
        snprintf(cmdbuf,sizeof(cmdbuf), "which \"%s\"", program) ;
        strcat(cmdbuf, " > /dev/null") ;
    }
    
    check = system(cmdbuf) ;
//     printf("Check = %d\n", check) ;
    
    if (check) {
        printf("Unable to find program '%s'. Aborting.\n", program) ;
        exit(check) ;
//     } else {
//         printf("The program '%s' exists.\n", program) ;
    }

}


// Close gnuplot
void c_close_gp_(){

    fclose(gp) ;
}
