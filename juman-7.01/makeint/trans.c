/*
==============================================================================
	trans.c
		1990/11/12/Mon Yutaka MYOKI(Nagao Lab., KUEE)
==============================================================================
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include 	"makeint.h"

#define         MRPH_BUF_MAX   1000
#define         KEITAISO_NUM_MAX  20
/*
------------------------------------------------------------------------------
	GLOBAL:
	definition of global variables
------------------------------------------------------------------------------
*/

extern char	*ProgName;
extern char	CurPath[FILENAME_MAX];
extern char	JumanPath[FILENAME_MAX];
extern CLASS	Class[CLASSIFY_NO + 1][CLASSIFY_NO + 1];
extern TYPE	Type[TYPE_NO];
extern FORM	Form[TYPE_NO][FORM_NO];
extern int	LineNo;
extern int	LineNoForError;

/*
------------------------------------------------------------------------------
	LOCAL:
	definition of global variables
------------------------------------------------------------------------------
*/

static 	MRPH 		mrph;
static  MRPH            mrph_buffer[MRPH_BUF_MAX];
static  int             mrph_buffer_num;

enum	ErrorCode	{NotList, IllegalForm, ConflictGobi,
			   NoMidasi, LongMidasi, 
			   NoYomi,   LongYomi, 
			   NoKatuyou,IllegalWeight,
			   LongRengo,ShortRengo,
                           NoKatuyoukei,HankakuChr};

/*
------------------------------------------------------------------------------
	PROCEDURE:
	<init_mrph>:
------------------------------------------------------------------------------
*/

static void init_mrph(MRPH *mrph_p)
{
     mrph_p->hinsi    = 0;
     mrph_p->bunrui   = 0;
     mrph_p->katuyou1 = 0;
     mrph_p->katuyou2 = 0;
     mrph_p->weight   = 0;
     mrph_p->con_tbl  = 0;
}

/*
------------------------------------------------------------------------------
	PROCEDURE:
	<print_mrph>:
------------------------------------------------------------------------------
*/

static void output_mrph(FILE *fp, MRPH *mrph_p)
{
    long       imiptr;

    /*登録データのフォーマット
      [品詞，細分類，活用型，活用形，重み，アドレス，意味(後回し)，読み] */
	
    numeral_encode(fp, mrph_p->hinsi);
    numeral_encode(fp, mrph_p->bunrui);
    numeral_encode(fp, mrph_p->katuyou1);
    numeral_encode(fp, mrph_p->katuyou2);
    numeral_encode(fp, mrph_p->weight);
    numeral_encode2(fp, mrph_p->con_tbl);
    hiragana_encode(fp, mrph_p->yomi);

    /* 意味情報の書き込み */
    if (!Null(mrph_p->imi))
	imi_print(fp, mrph_p->imi);
}

static void numeral_encode(FILE *fp, int num)
{
    if (num == atoi(RENGO_ID)) fputc(0xff, fp);
    else if (num < 0xf0-0x20) fputc(num+0x20, fp);
    else {
	fputc(num/(0xf0-0x20)+0xf0, fp);
	fputc(num%(0xf0-0x20)+0x20, fp);
    }
}

static void numeral_encode2(FILE *fp, int num)
{
    fputc((num+1)/(0x100-0x20)+0x20, fp);
    fputc((num+1)%(0x100-0x20)+0x20, fp);
}

static void hiragana_encode(FILE *fp, unsigned char *str)
{
    if (*str != '@') /* 空列でない場合 */
	fprintf(fp, "%s", str);
    fputc(0x20, fp);
}

static void imi_print(FILE *fp, CELL *cell)
{
    char buf[BUFSIZE];

    buf[0] = '\0';
    _imi_print(buf, cell);
    numeral_encode(fp, strlen(buf));
    fprintf(fp, "%s", buf);
}

static void _imi_print(char *buf, CELL *cell)
{
    if (Null(cell))
	strcat(buf, NILSYMBOL);
    else {
	switch (_Tag(cell)) {
	case CONS:
	    strcat(buf, "(");
	    _imi_print(buf, _Car(cell));
	    _imi_print_cdr(buf, _Cdr(cell));
	    strcat(buf, ")");
	    break;
	case ATOM:
	    strcat(buf, _Atom(cell));
	    break;
	default:
	    error(OtherError, "Illegal cell(in s_print)", EOA);
	}
    }
}

static void _imi_print_cdr(char *buf, CELL *cell)
{
     if (!Null(cell)) {
	 if (Consp(cell)) {
	     strcat(buf, " ");
	     _imi_print(buf, _Car(cell));
	     _imi_print_cdr(buf, _Cdr(cell));
	 } else {
	     strcat(buf, " ");
	     _imi_print(buf, cell);
	 }
     }
}

static void print_mrph(MRPH *mrph_p) /* by yamaji */
{
    mrph_buffer[mrph_buffer_num++] = *mrph_p;
    if (mrph_buffer_num == MRPH_BUF_MAX) error_in_trans(LongRengo , NULL);
}

static void print_mrph_loop(MRPH *mrph_p) /* by yamaji */
{
     int i;
     MRPH mrph_t;

     for ( i=1; Form[mrph_p->katuyou1][i].name; i++ ) {
	  if ( strlen(Form[mrph_p->katuyou1][i].gobi) ) {
	      mrph_t = *mrph_p;
	      mrph_t.katuyou2 = i;
	      strcpy(mrph_t.midasi , Form[mrph_p->katuyou1][i].gobi);
	      strcpy(mrph_t.yomi   , Form[mrph_p->katuyou1][i].gobi_yomi);
	      mrph_t.con_tbl += (i - 1);
	      print_mrph(&mrph_t);
	  }
     }
}

/*
------------------------------------------------------------------------------
	PROCEDURE:
	<error_in_trans>: local error processing
------------------------------------------------------------------------------
*/

static void error_in_trans(int n, void *c)
{
     fprintf(stderr, "\n%s: syntax error between line %d and %d.\n",
	     ProgName, LineNoForError, LineNo);
     switch (n) {
     case NotList:
	  fprintf(stderr, "\tis not list: ");
	  s_print(stderr, (CELL *)c);
	  break;
     case IllegalForm:
	  fprintf(stderr, "\tLIST for morpheme contains illegal form: ");
	  s_print(stderr, (CELL *)c);
	  break;
     case NoMidasi:
	  fprintf(stderr, 
		  "\tLIST for morpheme don't contain the list for MIDASI.\n");
	  s_print(stderr, (CELL *)c);
	  break;
     case LongMidasi:
	  fprintf(stderr, 
		  "\tMIDASI is too long: %s\n", (U_CHAR *)c);
	  break;
     case NoYomi:
	  fprintf(stderr, 
		  "\tLIST for morpheme don't contain the list for YOMI\n");
	  s_print(stderr, (CELL *)c);
	  break;
     case LongYomi:
	  fprintf(stderr,
		  "\tYOMI is too long: %s\n", (U_CHAR *)c);
	  break;
     case NoKatuyou:
	  fprintf(stderr, 
		  "\tLIST for morpheme don't contain the list for KATUYOU.\n");
	  s_print(stderr, (CELL *)c);
	  break;
     case ConflictGobi:
	  fprintf(stderr, 
		  "\tConflicting between <midasigo> and <katuyoukata>:");
	  fprintf(stderr, "%s\n", (U_CHAR *)c);
	  break;
     case IllegalWeight:
	  fprintf(stderr, 
		  "\t0.0 <= weight <= 25.6:");
	  s_print(stderr, (CELL *)c);
	  break;
     case LongRengo:
	  fprintf(stderr,
		  "\tRENGO is too long\n");
     case ShortRengo:
	  fprintf(stderr,
		  "\tRENGO is too short\n");
	  break;
     case NoKatuyoukei:
	  fprintf(stderr, 
		  "\tLIST for RENGO don't contain the list for KATUYOUKEI.:");
	  fprintf(stderr, "%s\n", (U_CHAR *)c);
	  break;
     case HankakuChr:
	  fprintf(stderr, "\tLIST for morpheme contains HANKAKU character: ");
	  fprintf(stderr, "%s\n", (U_CHAR *)c);
	  break;
     default:
	  error(ProgramError, "error_in_trans received an unexpected code.");
	  break;
     }

     my_exit(DicError);
}

/*
------------------------------------------------------------------------------
	FUNCTION:	** not used now ** 1992/9/10
	<midasi>: sub-routine of <trans>
------------------------------------------------------------------------------
*/

static U_CHAR *midasi(CELL *x)
{
     CELL	*y;
     U_CHAR	*s;

     if (Null(y = assoc(tmp_atom((U_CHAR *)"見出し語"), x)))
	  error_in_trans(NoMidasi, x);
     if (!Atomp(car(cdr(y)))) 
	  error_in_trans(IllegalForm, y);

     s = (U_CHAR *)_Atom(car(cdr(y)));
     if (hankaku_check(s)) error_in_trans(HankakuChr, s);

     if (strlen(s) > MIDASI_MAX)
	  error_in_trans(LongMidasi, s);

     return s;
}

/*
------------------------------------------------------------------------------
	FUNCTION:
	<midasi_list>: sub-routine of <trans>
------------------------------------------------------------------------------
*/

static CELL *midasi_list(CELL *x)
{
     CELL	*y;
     U_CHAR	*s;

     if (Null(y = assoc(tmp_atom((U_CHAR *)"見出し語"), x)))
	  error_in_trans(NoMidasi, x);

     return cdr(y);
}

/*
------------------------------------------------------------------------------
	FUNCTION
	<yomi>: sub-routine of <trans>
------------------------------------------------------------------------------
*/

static U_CHAR *yomi(CELL *x)
{
     CELL	*y;
     U_CHAR	*s;

     if (Null(y = assoc(tmp_atom((U_CHAR *)"読み"), x)))
	  error_in_trans(NoYomi, x);
     if (!Atomp(car(cdr(y))))
	  error_in_trans(IllegalForm, y);

     s = (U_CHAR *)_Atom(car(cdr(y)));
     if (hankaku_check(s)) error_in_trans(HankakuChr, s);

     if (strlen(s) > YOMI_MAX)
	  error_in_trans(LongYomi, s);

     return s;
}

/*
------------------------------------------------------------------------------
	FUNCTION:
	<katuyou1>: sun-routine of <trans>
------------------------------------------------------------------------------
*/

static int katuyou1(CELL *x)
{
    CELL	*y;
    int	i;

    if (Null(y = assoc(tmp_atom((U_CHAR *)"活用型"), x)))
      error_in_trans(NoKatuyou, x);
    if (!Atomp(car(cdr(y))))
      error_in_trans(IllegalForm, y);
    
    return get_type_id(_Atom(car(cdr(y))));
}

/*
------------------------------------------------------------------------------
	FUNCTION:
	<katuyou2>: sun-routine of <trans>
------------------------------------------------------------------------------
*/

static int katuyou2(CELL *x , int type)
{
    CELL	*y;
    int	i;

    if (Null(y = assoc(tmp_atom((U_CHAR *)"活用形"), x)))
      error_in_trans(NoKatuyou, x);
    if (!Atomp(car(cdr(y))))
      error_in_trans(IllegalForm, y);
    
    return get_form_id(_Atom(car(cdr(y))) , type);
}

/* for EDRdic '94.Mar */
/*
------------------------------------------------------------------------------
        FUNCTION:
        <edrconnect>: sub-routine of <trans>
------------------------------------------------------------------------------
*/

static CELL *edrconnect(CELL *x)
{
     CELL       *y;

     y = assoc(tmp_atom((U_CHAR *)"連接属性"), x);
     return car(cdr(y));
}

/*
------------------------------------------------------------------------------
	FUNCTION:
	<imi>: sub-routine of <trans>
------------------------------------------------------------------------------
*/

static CELL *imi(CELL *x)
{
     CELL	*y;

     y = assoc(tmp_atom((U_CHAR *)"意味情報"), x);
     return car(cdr(y));
}

/*
------------------------------------------------------------------------------
	PROCEDURE:
	<trim_yomi_gobi> <trim_midasi_gobi>: sub-routine of <trans>
------------------------------------------------------------------------------
*/

static void trim_yomi_gobi(MRPH *mrph_p)
{
     U_CHAR	*str = (U_CHAR *)"基本形";
     int	i;

     for (i = 1; strcmp(Form[mrph_p->katuyou1][i].name, str); i++);

     mrph_p->yomi[strlen(mrph_p->yomi) - 
		  strlen(Form[mrph_p->katuyou1][i].gobi_yomi)]='\0';
}

static void trim_midasi_gobi(MRPH *mrph_p)
{
    U_CHAR	*str = (U_CHAR *)"基本形";
    int	i;
    
    for (i = 1; strcmp(Form[mrph_p->katuyou1][i].name, str); i++);
    
    if (compare_end_str(mrph_p->midasi, Form[mrph_p->katuyou1][i].gobi))
      mrph_p->midasi[strlen(mrph_p->midasi) - 
		    strlen(Form[mrph_p->katuyou1][i].gobi)]='\0';
    else
      error_in_trans(ConflictGobi, mrph_p->midasi);
}

/*
------------------------------------------------------------------------------
	FUNCTION:
	<hankaku_check>: sub-routine of <trans>
------------------------------------------------------------------------------
*/

int hankaku_check(U_CHAR *s)
{
#ifdef IO_ENCODING_SJIS
    return(0);
#else
    while (*s) {
	if (*s < 0x80) return(1);
	s++;
    }
    return(0);
#endif
}

/*
------------------------------------------------------------------------------
	PROCEDURE:
	<trans>: translate from <fp_in> to <fp_out>
------------------------------------------------------------------------------
*/

void trans(FILE *fp_in, FILE *fp_out)
{
    CELL	*cell,*cell1;
    int         keitaiso_num;
    int         keitaiso_p[KEITAISO_NUM_MAX],keitaiso_c[KEITAISO_NUM_MAX];
    int         i,f;
    float	float_weight;
    int 	int_weight;
    MRPH        *mrph_p;
    U_CHAR      str_midasi[MIDASI_MAX*10];
    U_CHAR      str_yomi[YOMI_MAX*10];
    int         rengo_con_tbl;

/*    fprintf(fp_out, I_FILE_ID);*/
    
    LineNo = 1;
    while (! s_feof(fp_in)) {
	LineNoForError = LineNo;

	lisp_alloc_push();
	cell = s_read(fp_in);
	  
	if (Atomp(cell)) error_in_trans(NotList, cell);
	if (!Atomp(car(cell))) error_in_trans(IllegalForm, car(cell));
	
	if (get_hinsi_id(_Atom(car(cell))) == atoi(RENGO_ID)) {
            /* 連語の場合 */
	    keitaiso_num = mrph_buffer_num = 0;
	    cell1 = car(cdr(cell));
	    while (!Null(car(cell1))) {  /* 形態素情報を全て読み込む */
		keitaiso_p[keitaiso_num] = mrph_buffer_num;
		_trans(car(cell1) , TRUE);
		if (++keitaiso_num >= KEITAISO_NUM_MAX)
		    error_in_trans(LongRengo , NULL);
		cell1 = cdr(cell1);
	    }
	    keitaiso_p[keitaiso_num] = mrph_buffer_num;
	    if (keitaiso_num <= 1) error_in_trans(ShortRengo , NULL);

	    if (Null(cdr(cdr(cell)))) {
		int_weight = (int)(RENGO_DEFAULT_WEIGHT * 10 + 0.1);
	    } else {
		if (sscanf((char *)_Atom(car(cdr(cdr(cell)))),"%f",
			   &float_weight) == 0)
		    error_in_trans(IllegalForm, cell);
		int_weight = (int)(float_weight * 10 + 0.1);
		if (int_weight < 0 || int_weight > 256)
		    error_in_trans(IllegalWeight, cell);
	    }

	    /* 連語情報を出力する */
	    for (i = 0; i < keitaiso_num; i++) keitaiso_c[i] = keitaiso_p[i];
	    f = 1;
	    while (f) {
		str_midasi[0] = str_yomi[0] = '\0';
		for (i = 0 ; i < keitaiso_num ; i++) {
		    mrph_p = &mrph_buffer[keitaiso_c[i]];
		    if (strcmp(mrph_p->midasi , "@")) {
			strcat(str_midasi , mrph_p->midasi);
			strcat(str_yomi , mrph_p->yomi);
		    } /* 語幹なしなら何もつけない */
		    if (Class[mrph_p->hinsi][mrph_p->bunrui].kt) { /* 活用有 */
			if (mrph_p->katuyou2 == 0) {
			    if (i < keitaiso_num-1)
				error_in_trans(NoKatuyoukei , mrph_p->midasi);
			} else {
			    strcat(str_midasi , Form[mrph_p->katuyou1]
				   [mrph_p->katuyou2].gobi);
			    strcat(str_yomi ,   Form[mrph_p->katuyou1]
				   [mrph_p->katuyou2].gobi_yomi);
			}
		    }
		    if (strlen(str_midasi) > MIDASI_MAX)
			error_in_trans(LongMidasi, str_midasi);
		    if (strlen(str_yomi) > YOMI_MAX)
			error_in_trans(LongYomi, str_yomi);
		}

		/* 連語として連接規則が記述されているか調べる */
		mrph_p = &mrph_buffer[keitaiso_c[keitaiso_num-1]];
		mrph.katuyou1 = mrph_p->katuyou1;
		strcpy(mrph.midasi , str_midasi);
		if (Class[mrph_p->hinsi][mrph_p->bunrui].kt != 0 &&
		    mrph_p->katuyou2 == 0) {
		    for (i = 1;
			 strcmp(Form[mrph_p->katuyou1][i].name,"基本形");i++);
		    strcat(mrph.midasi , Form[mrph_p->katuyou1][i].gobi);
		}
		check_table_for_rengo(&mrph);

		fprintf(fp_out, "%s\t", str_midasi);
		numeral_encode(fp_out, atoi(RENGO_ID));
		numeral_encode(fp_out, keitaiso_num);
		numeral_encode(fp_out, 0);
		numeral_encode(fp_out, 0);
		numeral_encode(fp_out, int_weight);
		numeral_encode2(fp_out, mrph.con_tbl);
		hiragana_encode(fp_out, str_yomi);

		/* 中身の形態素情報を出力する */
		for (i = 0 ; i < keitaiso_num ; i++) {
		    if (!strcmp(mrph_buffer[keitaiso_c[i]].midasi, "@"))
			fprintf(fp_out, "  ");
		    else
			fprintf(fp_out, " %s ",
				mrph_buffer[keitaiso_c[i]].midasi);
		    output_mrph(fp_out , &mrph_buffer[keitaiso_c[i]]);
		}
		fprintf(fp_out, "\n");
		
		i = 0;
		while (1) { /* 次の組み合わせ方で連語を構成する */
		    if (++keitaiso_c[i] == keitaiso_p[i+1]) {
			keitaiso_c[i] = keitaiso_p[i];
			if (++i == keitaiso_num) {f = 0; break;}
		    } else break;
		}
	    }
	} else {
            /* 形態素の場合 */
	    mrph_buffer_num = 0;
	    _trans(cell , FALSE);
	    for (i = 0 ; i < mrph_buffer_num ; i++) {
		fprintf(fp_out, "%s\t", mrph_buffer[i].midasi);
		output_mrph(fp_out , &mrph_buffer[i]);
		fprintf(fp_out, "\n");
	    }
	}
	lisp_alloc_pop();
    }
}
	  
static void _trans(CELL *cell , int rengo_p)
{
    CELL   *main_loop, *main_block, *sub_loop, *sub_block;
    MRPH   *mrph_p;

    mrph_p = &mrph;
    init_mrph(mrph_p);

    mrph_p->hinsi = get_hinsi_id(_Atom(car(cell)));	/* 形態品詞 */
	  
    main_loop = cdr(cell);
    while (!Null(main_block = car(main_loop))) {

	/* 細分類がある場合 */
	if (Atomp(car(main_block))) {
	    mrph_p->bunrui =
		get_bunrui_id(_Atom(car(main_block)), mrph_p->hinsi);
	    sub_loop = cdr(main_block);
	    while (!Null(sub_block = car(sub_loop))) {
		__trans(sub_block, mrph_p, rengo_p);
		sub_loop = cdr(sub_loop);
	    }
	} 

	/* 細分類がない場合 */
	else {
	    mrph_p->bunrui = 0;
	    __trans(main_block, mrph_p, rengo_p);
	}

	main_loop = cdr(main_loop);
    }
}

static void __trans(CELL *block, MRPH *mrph_p, int rengo_p)
{
    CELL 	*loop, *midasi_cell;
    U_CHAR 	*midasi_cp = NULL;
    float	float_weight;
    int 	int_weight;
    CELL        *connect_cell;                    /* EDRdic '94.Mar */

    strcpy(mrph_p->yomi, yomi(block))  ;		/* 読み     */
    mrph_p->imi = imi(block);				/* 意味情報 */
    if (Class[mrph_p->hinsi][mrph_p->bunrui].kt) {
	mrph_p->katuyou1 = katuyou1(block);		/* 活用型   */
	if (rengo_p) mrph_p->katuyou2 = katuyou2(block , mrph_p->katuyou1);
	else mrph_p->katuyou2 = 0;	             	/* 活用 */
    } else
	mrph_p->katuyou1 = 0;
    
    if (Class[mrph_p->hinsi][mrph_p->bunrui].kt)
	trim_yomi_gobi(mrph_p);

    loop = midasi_list(block);				/* 見出し語 */
    while (!Null(midasi_cell = car(loop))) {

	/* (見出し語 ×××) の場合 */
	if (Atomp(midasi_cell)) {
	    midasi_cp = _Atom(midasi_cell);
	    mrph_p->weight = MRPH_DEFAULT_WEIGHT;
	} 
	
	/* (見出し語 (××× weight)) の場合 */
	else if (Atomp(car(midasi_cell))) {
	    midasi_cp = _Atom(car(midasi_cell));

	    if (Null(cdr(midasi_cell)))
		mrph_p->weight = MRPH_DEFAULT_WEIGHT;
	    else if (Atomp(car(cdr(midasi_cell)))) {
		if (sscanf((char *)_Atom(car(cdr(midasi_cell))),"%f",
			   &float_weight) == 0)
		    error_in_trans(IllegalForm, midasi_cell);
		int_weight = (int)(float_weight * MRPH_DEFAULT_WEIGHT + 0.1);
		if (int_weight < 0 || int_weight > 256)
		    error_in_trans(IllegalWeight, midasi_cell);
		mrph_p->weight = (U_CHAR)int_weight;
	    }
	    else 
		error_in_trans(IllegalForm, midasi_cell);
	} else {
	    error_in_trans(IllegalForm, midasi_cell);	      
	}  

	if (hankaku_check(midasi_cp)) error_in_trans(HankakuChr, midasi_cp);
	if (strlen(midasi_cp) > MIDASI_MAX)
	    error_in_trans(LongMidasi, midasi_cp);
	strcpy(mrph_p->midasi, midasi_cp);

	if ( Null(connect_cell = edrconnect(block)) ){
            check_table(mrph_p);                            /* 連接情報 */
        }
        else {                                        /* for EDRdic '94.Mar */
            check_edrtable(mrph_p, connect_cell);
        }

	if (Class[mrph_p->hinsi][mrph_p->bunrui].kt) {
	    trim_midasi_gobi(mrph_p);
	    if ( strlen(mrph_p->midasi) )
		print_mrph(mrph_p);
	    else {
		if (rengo_p) {
		    strcpy(mrph_p->midasi , "@");
		    strcpy(mrph_p->yomi   , "@");
		    print_mrph(mrph_p);
		} else
		    print_mrph_loop(mrph_p);
		/* 語幹が無くなる場合は全ての活用形を登録 */
	    }
	} else
	    print_mrph(mrph_p);
	
	loop = cdr(loop);
    }
}

