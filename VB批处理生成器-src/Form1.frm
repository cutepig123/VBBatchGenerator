VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "VB批处理生成器(hjs00@126.com)"
   ClientHeight    =   3195
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   OLEDropMode     =   1  'Manual
   ScaleHeight     =   3195
   ScaleWidth      =   4680
   StartUpPosition =   1  'CenterOwner
   Begin VB.CommandButton CmdView 
      Caption         =   "View"
      Height          =   255
      Left            =   1200
      TabIndex        =   2
      Top             =   2880
      Width           =   615
   End
   Begin VB.CommandButton CmdClear 
      Caption         =   "Clear"
      Height          =   255
      Left            =   480
      TabIndex        =   1
      Top             =   2880
      Width           =   615
   End
   Begin VB.TextBox Text1 
      Height          =   2535
      Left            =   240
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   0
      Text            =   "Form1.frx":0E42
      Top             =   240
      Width           =   4215
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "把文件或文件夹拖到这里就行"
      Height          =   195
      Left            =   2040
      TabIndex        =   3
      Top             =   2880
      Width           =   2340
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Declare Function SendMessage Lib "USER32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long


Private Declare Function SetWindowPos Lib "USER32" (ByVal hwnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal Y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
Private Declare Function SendMessageLong Lib _
                          "USER32" Alias "SendMessageA" _
                          (ByVal hwnd As Long, _
                          ByVal wMsg As Long, _
                          ByVal wParam As Long, _
                          ByVal lParam As Long) As Long
                          
Private Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long


Private Const EM_GETLINECOUNT = &HBA
Private Const EM_GETLINE = &HC4
Private Const EM_LINEFROMCHAR = &HC9
Private Const HWND_TOPMOST& = -1
' 将窗口置于列表顶部，并位于任何最顶部窗口的前面
Private Const SWP_NOSIZE& = &H1
' 保持窗口大小
Private Const SWP_NOMOVE& = &H2
Private Const HWND_NOTOPMOST = -2
Const CB_GETCURSEL = &H147
Const CB_SETCURSEL = &H14E
Const LB_SETCURSEL = &H186
Const LB_GETCURSEL = &H188

Dim cnt As Long
Dim cntPdf As Long
Dim cntFdr As Long
Dim strFname As String
Dim strCommandTemplate As String
Dim strFileExt As String

Private Sub CmdClear_Click()
    Text1.Text = ""
End Sub

Private Sub CmdView_Click()
    Shell "notepad.exe " & strFname, vbNormalFocus
End Sub

Function IsCppFile(szFile As String, szFileExtArr As String) As Boolean
    Dim FileExt
    Dim i As Long
    Dim l As Long
    
    FileExt = Split(szFileExtArr, ";")
    For i = LBound(FileExt) To UBound(FileExt)
        l = Len(FileExt(i) & "")
        If l > 0 Then
            If LTrim(Right(szFile, l)) = LTrim(FileExt(i) & "") Then
                IsCppFile = True
                'MsgBox FileExt(i) & ""
                Exit Function
            End If
        End If
    Next
    
    IsCppFile = False
End Function
Private Sub Form_Load()
    'strCommandTemplate = """%APPPATH%\cpp2h.exe"" ""%1"" -t FCN -e 1"
    
    'strFileExt = ".c;.cpp"
    
    strCommandTemplate = Space(255)
    strFileExt = Space(255)
    GetPrivateProfileString "ccpToh.exe", "strCommandTemplate", "", strCommandTemplate, 255, App.Path & "\ccpToh.ini"
    GetPrivateProfileString "ccpToh.exe", "strFileExt", "", strFileExt, 255, App.Path & "\ccpToh.ini"
    strCommandTemplate = Trim(strCommandTemplate)
    strFileExt = Trim(strFileExt)
    If Len(strFileExt) < 2 Or Len(strCommandTemplate) < 2 Then
        MsgBox "出错,程序即将退出:" & vbCrLf & "请检查ccpToh.ini中strFileExt,strCommandTemplate字段的设置", vbCritical, "Error!"
        End
    End If
'    MsgBox Asc(strCommandTemplate)
'    MsgBox Len(strCommandTemplate)
'    MsgBox strFileExt
'    MsgBox Len(strFileExt)

    '以下为测试用
    'MsgBox strCommandTemplate
    'MsgBox IsCppFile("ssxs.c", strFileExt)
    'MsgBox LTrim(".XXD123cd")
    strCommandTemplate = Replace(strCommandTemplate, "%APPPATH%", App.Path)
    strFname = App.Path & "\1.bat"
    cnt = 0
    cntPdf = 0
    cntFdr = 0
    SetWindowPos Me.hwnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE Or SWP_NOSIZE
End Sub

Private Sub Form_OLEDragDrop(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, x As Single, Y As Single)
    Dim i As Long
    If Data.GetFormat(vbCFFiles) = True Then
        cnt = 0
        cntPdf = 0
        cntFdr = 0
        Text1 = ""
        'txtName = strFname & ".txt"
        
'        On Error Resume Next
'        If Dir(strFname) <> "" Then Kill strFname
'        If Error Then Err.Clear
        Open strFname For Output As #1
        'If Dir(txtName) <> "" Then Kill txtName
        
        For i = 1 To Data.Files.Count
            DoEvents
            GetSubFiles Data.Files(i)
            Label1.Caption = cntPdf & "/" & cnt & "文件，" & cntFdr & "文件夹"
            DoEvents

        Next


        Print #1, "pause" & vbCrLf
        Close #1

        '
        Text1 = "共" & cnt & "个文件," & cntPdf & "个" & strFileExt & "文件" & vbCrLf & Text1
        If cntPdf > 0 Then Shell strFname, vbNormalFocus
        'If Dir(txtName) <> "" Then Shell "cmd.exe /c type " & txtName & " | find /i ""hasn't been encrypted"""
    End If
End Sub

Sub GetSubFiles(Foldername As String)
    Dim fso As New FileSystemObject
    Dim Fdr As Folder
    Dim subFdr As Folder
    Dim subFile As File
    'Dim txtName
    'txtName = strFname & ".txt"
    
    If fso.FolderExists(Foldername) = True Then
        Text1 = Foldername & vbCrLf & Text1
        cntFdr = cntFdr + 1
        If cntFdr Mod 10 = 0 Then Label1.Caption = cntPdf & "/" & cnt & "文件，" & cntFdr & "文件夹"
        
        If Len(Text1) > 20000 Then Text1.Text = ""
        Set Fdr = fso.GetFolder(Foldername)
        For Each subFdr In Fdr.SubFolders
            GetSubFiles subFdr.Path
        Next

        For Each subFile In Fdr.Files
            'If LCase(Right(subFile.Path, 3)) = "cpp" Or LCase(Right(subFile.Path, 1)) = "c" Then
            If IsCppFile(subFile.Path, strFileExt) Then
                Print #1, Replace(strCommandTemplate, "%1", subFile.Path) & vbCrLf
                cntPdf = cntPdf + 1
            End If
            DoEvents
        Next
        cnt = cnt + Fdr.Files.Count
    Else
        cnt = cnt + 1
        'If LCase(Right(Foldername, 3)) = "cpp" Or LCase(Right(Foldername, 1)) = "c" Then
        If IsCppFile(Foldername, strFileExt) Then
            cntPdf = cntPdf + 1
            Print #1, Replace(strCommandTemplate, "%1", Foldername) & vbCrLf
            'Print #1, """" & App.Path & "\pdfdecrypt.exe""  -i """ & Foldername & """ -o """ & Foldername & """ -l """ & txtName & """" '& vbCrLf
        End If
    End If

    DoEvents
    Set fso = Nothing
End Sub

Private Sub Form_Resize()
    Resize_ALL Me
End Sub

Private Sub Form_Terminate()
    End
End Sub

Private Sub Form_Unload(Cancel As Integer)
    End
End Sub
