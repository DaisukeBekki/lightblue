/*
==============================================================================
	makeint.c
		ユーザ辞書を maketree 可読形式（バイナリファイル）に変換する
		1990/11/09/Fri	Yutaka MYOKI(Nagao Lab., KUEE)
		1991/01/08/Tue	Ver 1.00
==============================================================================
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include 	"makeint.h"

/*
------------------------------------------------------------------------------
	GLOBAL:
	definition of global variables
------------------------------------------------------------------------------
*/

extern char	*ProgName;
extern FILE	*Jumanrc_Fileptr;
extern FILE	*Cha_stderr;

char		CurPath[FILENAME_MAX];
char		JumanPath[FILENAME_MAX];

/*
------------------------------------------------------------------------------
	PROCEDURE:
	<usage>: print usage on "stderr", and "exit"
------------------------------------------------------------------------------
*/

void usage(void)
{
     fprintf(stderr, "<usage>: %s <filename ...>\n", ProgName);
     my_exit(ConfigError);
}

/*
------------------------------------------------------------------------------
	PROCEDURE:
	<translate>: call <trans>
------------------------------------------------------------------------------
*/

void translate(char *arg)
{
     char	filename[FILENAME_MAX], filename_path[FILENAME_MAX];
     char	ans[NAME_MAX_];
     FILE	*fp_in, *fp_out;

     strcpy(filename, arg);
     /* Win98 は *.DIC まで反応してしまうので拡張子の処理は省略 */
#ifndef _WIN32
     append_postfix(filename, S_POSTFIX);
#endif

     while (1) {
	  if ((fp_in = pathfopen(filename, "r", ""     , filename_path))
	      != NULL )	break;
	  if ((fp_in = pathfopen(filename, "r", CurPath, filename_path))
	      != NULL )	break; 
/*
	  if ((fp_in = pathfopen(filename, "r", JumanPath, filename_path))
	      != NULL )	break;
*/
	  error(OpenError, "can't open", filename, ".", EOA);
     }
	  
     fprintf(stderr, "%s parsing... ", filename_path);

     change_postfix(filename_path, S_POSTFIX, I_POSTFIX);
#ifdef _WIN32
     fp_out = my_fopen(filename_path, "wb");
#else
     fp_out = my_fopen(filename_path, "w");
#endif
     if (fp_out != NULL) {
	  trans(fp_in, fp_out);	
	  fprintf(stderr, "done.\n\n");
     }
     fclose(fp_out);
     fclose(fp_in);
}

/*
------------------------------------------------------------------------------
	FUNCTION
	<main>: main routine
------------------------------------------------------------------------------
*/

int main(int argc, char *argv[])
{
     long	t0, t1;
     long	p0, p1;
     int	dt, i;
     float	dp;

     ProgName = argv[0];
     if (argc == 1) usage();

     if ((argc >= 3)&&(strncmp(argv[1], "-r", 2) == 0)) {
	 i = 3;
	 set_jumanrc_fileptr(argv[2], FALSE, FALSE);
     } else {
	 i = 1;
	 set_jumanrc_fileptr(NULL, TRUE, FALSE);
     }
     if (Jumanrc_Fileptr) {
	 set_jumangram_dirname();
     }

     getpath(CurPath, JumanPath);

     grammar(stderr);
     katuyou(stderr);
     connect_table(stderr);

     time(&t0); p0 = clock();

     for (; i < argc; i++)
	 translate(argv[i]);

     p1 = clock(); time(&t1);

     dt = t1 - t0; dp = (float)(p1 - p0);
     print_execute_time(stderr, dt, dp);

     exit(NormalExit);
}
