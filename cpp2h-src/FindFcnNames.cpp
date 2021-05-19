// FindFcnNames.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <windows.h>
#include <iostream>
using namespace std;
#define CHECK(x) if(!(x)) \
				{printf("Error:%s\n",#x);return;} \
				else {printf("OK:%s\n",#x);}

int g_Line=0;
int CntFcn=0;

//表示需要生成的内容
enum {FCN,	//表示仅仅生成函数
	NOTES,	//表示仅仅生成注释
	ALL		//表示生成函数和注释
};

/*掠过空格，并且移动指针p使之指向第一个非空格*/
void SkipBlank(char *&p)
{
	while(*p==' '||*p=='\t')p++;
}

//函数功能:根据文件指针fp，判断当前行有没有函数，如果有，就设置szFcnName为函数名，并返回1，否则返回0或者-1
//返回-1表示文件结束，1表示成功，0表示失败
int GetFcnName(FILE *fp,string & szFcnName)
{
	char ch[2000];
	char *p;
	
	if(fgets(ch,2000,fp)==0)
	{
		printf("文件结束\n");
		return -1;
	}
	g_Line++;
	
	p=ch;
	SkipBlank(p);
	char *p2=p;

	char tmp[20];
	sprintf(tmp,"//(Line %d)\n",g_Line);
	szFcnName=tmp;
	
	//处理return,throw
	if(strncmp(p,"return",strlen("return"))==0)
		return 0;
	if(strncmp(p,"throw",strlen("throw"))==0)
		return 0;;
	if(strncmp(p,"else",strlen("else"))==0)
		return 0;;
	
	//统计cntPre，必须不少于2个
	int cntPre=0;
	while(1)
	{
		SkipBlank(p);
		if(!isalpha(*p)){
			break;
		}
		while(isalnum(*p)||*p==':')p++;
		SkipBlank(p);
		cntPre++;
	}
	
	if(cntPre<2)
	{
		//printf("cntPre <2\n");
		return 0;;
	}
	
	//（
	if(*p!='('){
		//	printf("Not (:%s",p);
		return 0;;//(
	}
	p++;
	
	//处理括号必须配对
	int cntKuoHao=1;
	
	while(1)
	{
		//*p /*&& (strstr(p,"(")|| strstr(p,")"))*/ && *p!='{' && *p!='}'
		if(cntKuoHao==0)//括号一致，说明找到函数了
		{
			*p++=';';
			*p=0;
			szFcnName+=p2;
			return 1;
		}
		
		if(szFcnName.length()>400)
		{
			printf("出错了，怎么找到了一个这么长的函数?\n(%d) %s\n",g_Line,szFcnName.c_str());
		//	printf("调试:cntKuoHao=%d,p=%s,str=%s---\n",cntKuoHao,p,ch);
			exit(0);
			return -1;
		}
		
		if(cntKuoHao<0)
		{
			printf("出错了，cntKuoHao=%d? \n",cntKuoHao);
			exit(0);
			return -1;
		}

		if(*p=='('){
			cntKuoHao++;
		//	printf("调试:cntKuoHao=%d,p=%s,str=%s---\n",cntKuoHao,p,ch);
		}
		else if(*p==')'){
			
			cntKuoHao--;
		//	printf("调试:cntKuoHao=%d,p=%s,str=%s---\n",cntKuoHao,p,ch);
		}
		else if(*p=='{'||*p=='}')
		{
			printf("函数不对，可能是原文件有错误");
		}
		else if(*p==0)
		{
			szFcnName+=p2;
			if(fgets(ch,2000,fp)==0)
				return -1;
		//	printf("调试：读入%s",ch);
			p2=ch;
			p=ch;
			
			g_Line++;
		}

		p++;
	}
	
	
	return 0;
	
}

/*
    //函数功能:根据文件指针fp，判断当前行有没有注释，如果有，
	就设置szFcnName为注释的内容，并返回1，否则返回0或者-1
  */
//返回-1表示文件结束，1表示成功，0表示失败
int GetNotes(FILE *fp,string &strNotes)
{
	char ch[2000];
	char *p;

	char tmp[20];
	sprintf(tmp,"//(NotesLine:%d)\n",g_Line);
	strNotes=tmp;
	
	if(fgets(ch,2000,fp)==0)
		return -1;
	
	g_Line++;

	if((p=strstr(ch,"//"))!=0)
	{
		strNotes+=p;
		
		return 1;
	}
	else if((p=strstr(ch,"/*"))!=0)
	{
		char *p2=p;
		printf("找到/*,(%d) %s",g_Line,ch);
		
		//查找*/
		if((p2=strstr(p+2,"*/"))!=0)
		{
			char substr[2000];
			strncpy(substr,p,p2+2-p);substr[p2+2-p]=0;
			strNotes+=substr;
			strNotes+="\n";
			return 1;
		}
		else
		{
			strNotes+=p;
			while(1)
			{
				if(fgets(ch,2000,fp)==0)
					return -1;
				g_Line++;
				
				//查找*/，如果找到就结束
				if((p2=strstr(ch,"*/"))!=0)
				{
					char substr[2000];
					strncpy(substr,ch,p2+2-ch);substr[p2+2-ch]=0;
					strNotes+=substr;
					strNotes+="\n";
					return 1;
				}
				else
					strNotes+=ch;
			}
			return 0;
		}

		return 1;
	}

	return 0;
}

//主调函数，根据输入的文件名和类型，生成相应的头文件
void ParseFile(char *szFileIn,char *szFileOut,int type=ALL,int bExtern=1)
{
	FILE *fin,*fo;
	
	CHECK(fin=fopen(szFileIn,"r"));
	CHECK(fo=fopen(szFileOut,"w"));
	
	
	while (1)
	{
		//读入每一行，找到函数的定义,空格+字母+字母数字+空格+(+not)+)+not;
		string szFcnname;
		int posOld;
		int LineOld;
		int re;
		
		//printf("---%s",ch);
		if(type==ALL)
		{
			posOld=ftell(fin);
			LineOld=g_Line;
		}
		
		if(type==FCN||type==ALL)
		{
			re=GetFcnName(fin,szFcnname);
			if(re==1)
			{
				CntFcn++;
				if(bExtern)
					fprintf(fo,"extern %s\n",szFcnname.c_str());
				else
					fprintf(fo,"%s\n",szFcnname.c_str());
			}
			else if(re==-1)
				break;
		}

		if(type==ALL)
		{
			g_Line=LineOld;
			fseek(fin,posOld,0);
		}	
		
		if(type==NOTES||type==ALL)
		{
			string strNotes;
			re=GetNotes(fin,strNotes);
			if(re==1)
				fprintf(fo,"%s",strNotes.c_str());
			else if(re==-1)
				break;
		}
	}

	fprintf(fo,"/*共函数%d个*/\n",CntFcn);
	
	fclose(fin);
	fclose(fo);
}

void Usage(char *s)
{
	printf("Usage:\n");
	printf("	%s cpp_file_name [-t type] [-e1 | -e0] \n",s);
	printf("Note:type can be ALL,FCN,NOTES.By default,type=FCN\n");
}

int main(int argc, char* argv[])
{
	printf("Hello World!\n");
	string sFileOut;
	int type=FCN;
	int bextern=1;
	
	if(argc<2)
	{
		Usage(*argv);
		return -1;
	}
	
	{
				sFileOut=argv[1];
				sFileOut+=".h";
	}

	for (int i=1;i<argc;i++)
	{
		switch(argv[i][0]) 
		{
		case '-':
			{
				if(argv[i][1]=='t')
				{
					if(i+1>=argc)
					{
						Usage(*argv);return -2;
					}
					
					i++;

					if(stricmp(argv[i],"ALL")==0)
						type=ALL;
					else if(stricmp(argv[i],"FCN")==0)
						type=FCN;
					else if(stricmp(argv[i],"NOTES")==0)
						type=NOTES;
					else 
					{
						Usage(*argv);
						return -3;
					}
					printf("type:%s\n",argv[i+1]);
				}
				else if(argv[i][1]=='e')
				{
					if(i+1>=argc)
					{
						Usage(*argv);return -1;
					}

					i++;

					bextern=(argv[i][0]=='1');
					printf("bextern:%d\n",bextern);
				}
				else 
				{
					Usage(*argv);
					return -4;
				}
			}
			break;
		default:
			{
//				Usage(*argv);
//				return -5;
			}
		
		}
	}
	
	printf("fileout:%s\n",sFileOut.c_str());
	ParseFile(argv[1],(char*)(sFileOut.c_str()),type,bextern);
	
	return 0;
}


