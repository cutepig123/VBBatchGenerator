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

//��ʾ��Ҫ���ɵ�����
enum {FCN,	//��ʾ�������ɺ���
	NOTES,	//��ʾ��������ע��
	ALL		//��ʾ���ɺ�����ע��
};

/*�ӹ��ո񣬲����ƶ�ָ��pʹָ֮���һ���ǿո�*/
void SkipBlank(char *&p)
{
	while(*p==' '||*p=='\t')p++;
}

//��������:�����ļ�ָ��fp���жϵ�ǰ����û�к���������У�������szFcnNameΪ��������������1�����򷵻�0����-1
//����-1��ʾ�ļ�������1��ʾ�ɹ���0��ʾʧ��
int GetFcnName(FILE *fp,string & szFcnName)
{
	char ch[2000];
	char *p;
	
	if(fgets(ch,2000,fp)==0)
	{
		printf("�ļ�����\n");
		return -1;
	}
	g_Line++;
	
	p=ch;
	SkipBlank(p);
	char *p2=p;

	char tmp[20];
	sprintf(tmp,"//(Line %d)\n",g_Line);
	szFcnName=tmp;
	
	//����return,throw
	if(strncmp(p,"return",strlen("return"))==0)
		return 0;
	if(strncmp(p,"throw",strlen("throw"))==0)
		return 0;;
	if(strncmp(p,"else",strlen("else"))==0)
		return 0;;
	
	//ͳ��cntPre�����벻����2��
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
	
	//��
	if(*p!='('){
		//	printf("Not (:%s",p);
		return 0;;//(
	}
	p++;
	
	//�������ű������
	int cntKuoHao=1;
	
	while(1)
	{
		//*p /*&& (strstr(p,"(")|| strstr(p,")"))*/ && *p!='{' && *p!='}'
		if(cntKuoHao==0)//����һ�£�˵���ҵ�������
		{
			*p++=';';
			*p=0;
			szFcnName+=p2;
			return 1;
		}
		
		if(szFcnName.length()>400)
		{
			printf("�����ˣ���ô�ҵ���һ����ô���ĺ���?\n(%d) %s\n",g_Line,szFcnName.c_str());
		//	printf("����:cntKuoHao=%d,p=%s,str=%s---\n",cntKuoHao,p,ch);
			exit(0);
			return -1;
		}
		
		if(cntKuoHao<0)
		{
			printf("�����ˣ�cntKuoHao=%d? \n",cntKuoHao);
			exit(0);
			return -1;
		}

		if(*p=='('){
			cntKuoHao++;
		//	printf("����:cntKuoHao=%d,p=%s,str=%s---\n",cntKuoHao,p,ch);
		}
		else if(*p==')'){
			
			cntKuoHao--;
		//	printf("����:cntKuoHao=%d,p=%s,str=%s---\n",cntKuoHao,p,ch);
		}
		else if(*p=='{'||*p=='}')
		{
			printf("�������ԣ�������ԭ�ļ��д���");
		}
		else if(*p==0)
		{
			szFcnName+=p2;
			if(fgets(ch,2000,fp)==0)
				return -1;
		//	printf("���ԣ�����%s",ch);
			p2=ch;
			p=ch;
			
			g_Line++;
		}

		p++;
	}
	
	
	return 0;
	
}

/*
    //��������:�����ļ�ָ��fp���жϵ�ǰ����û��ע�ͣ�����У�
	������szFcnNameΪע�͵����ݣ�������1�����򷵻�0����-1
  */
//����-1��ʾ�ļ�������1��ʾ�ɹ���0��ʾʧ��
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
		printf("�ҵ�/*,(%d) %s",g_Line,ch);
		
		//����*/
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
				
				//����*/������ҵ��ͽ���
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

//��������������������ļ��������ͣ�������Ӧ��ͷ�ļ�
void ParseFile(char *szFileIn,char *szFileOut,int type=ALL,int bExtern=1)
{
	FILE *fin,*fo;
	
	CHECK(fin=fopen(szFileIn,"r"));
	CHECK(fo=fopen(szFileOut,"w"));
	
	
	while (1)
	{
		//����ÿһ�У��ҵ������Ķ���,�ո�+��ĸ+��ĸ����+�ո�+(+not)+)+not;
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

	fprintf(fo,"/*������%d��*/\n",CntFcn);
	
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


