Public Class Form1
    '檢定回文
    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        Dim all, lla, m1, n2, go
        'Dim 原始資料 反轉資料 商數 餘數 運算資料
        FileOpen(1, "C:\Users\dora0\Desktop\NTVS\丙級\軟設丙\測資\1.txt", OpenMode.Input)
        Input(1, all)
        TextBox6.Clear()
        FileClose(1)
        go = all
        For i = 1 To 9
            m1 = go \ 10
            n2 = go Mod 10
            lla = lla & n2
            If m1 = 0 Then
                Exit For
            Else
                go = m1
            End If
        Next
        If all = lla Then
            TextBox6.Text = "第一題結果:" & all & " is a palindrome"
        Else
            TextBox6.Text = "第一題結果:" & all & " is not a palindrome"
        End If
    End Sub
    '檢定三角形(for)
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim data
        FileOpen(2, "C:\Users\dora0\Desktop\NTVS\丙級\軟設丙\測資\2.txt", OpenMode.Input)
        Input(2, data)
        FileClose(2)
        For x = 1 To data
            For y = 1 To x
                TextBox3.Text = TextBox3.Text & y
            Next
            TextBox3.Text = TextBox3.Text & vbNewLine
        Next
    End Sub
    '檢定三角形(do while)
    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim data
        FileOpen(2, "C:\Users\dora0\Desktop\NTVS\丙級\軟設丙\測資\2.txt", OpenMode.Input)
        Input(2, data)
        FileClose(2)
        TextBox4.Clear()

        Dim x = 1
        Do While x <= data
            Dim y = 1
            Do While y <= x
                TextBox4.Text = TextBox4.Text & y
                y = y + 1
            Loop
            TextBox4.Text = TextBox4.Text & vbNewLine
            x = x + 1
        Loop

    End Sub
    '檢定三角形(loop while)
    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Dim Data
        FileOpen(2, "C:\Users\dora0\Desktop\NTVS\丙級\軟設丙\測資\2.txt", OpenMode.Input)
        Input(2, Data)
        FileClose(2)
        TextBox5.Clear()
        Dim x = 1
        Do
            Dim y = 1
            Do
                TextBox5.Text = TextBox5.Text & y
                y = y + 1
            Loop While y <= x
            TextBox5.Text = TextBox5.Text & vbNewLine
            x = x + 1
        Loop While x <= Data
    End Sub

    '檢定質數
    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        Dim data, num
        FileOpen(3, "C:\Users\dora0\Desktop\NTVS\丙級\軟設丙\測資\3.txt", OpenMode.Input)
        Input(3, data)
        FileClose(3)
        For i = 1 To data
            If data Mod i = 0 Then
                num = num + 1
            End If
        Next
        If num = 2 Then
            TextBox7.Text = data & "質數"
        Else
            TextBox7.Text = data & "不是質數"
        End If
    End Sub
    'BMi計算
    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        Dim H(3), W(3), BMI(3)
        FileOpen(4, "C:\Users\dora0\Desktop\NTVS\丙級\軟設丙\測資\4.txt", OpenMode.Input)
        TextBox8.Clear()
        For I = 1 To 3
            Input(4, H(I))
            Input(4, W(I))
            BMI(I) = W(I) / (H(I) / 100) ^ 2
        Next
        FileClose(4)
        Dim x, y, z
        For x = 1 To 2
            For y = 1 To 2
                If BMI(y) > BMI(y + 1) Then
                    z = BMI(y)
                    BMI(y) = BMI(y + 1)
                    BMI(y + 1) = z
                End If
            Next
        Next
        Dim a As Integer
        a = BMI(1)
        If a >= 20 And a <= 25 Then
            TextBox8.Text = "第四題結果 : 最小BMI值=" & a & "，正常"
        Else
            TextBox8.Text = "第四題結果 : 最小BMI值=" & a & "，不正常"
        End If
    End Sub
    '陣列
    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        Dim a(4), b(4), c(4)
        FileOpen(5, "C:\Users\dora0\Desktop\NTVS\丙級\軟設丙\測資\5.txt", OpenMode.Input)
        TextBox9.Clear()
       
        For x = 1 To 4
            Input(5, a(x))
        Next
        For x = 1 To 4
            Input(5, b(x))
        Next
        For x = 1 To 4
            c(x) = a(x) + b(x)
        Next
        FileClose(5)
        TextBox9.Text = "第五題結果為 : " & vbNewLine & "[" & c(1) & "    " & c(2) & "]" & vbNewLine & "[" & c(3) & "    " & c(4) & "]"

    End Sub
    '二維陣列(不建議使用)
    Private Sub Button10_Click(sender As Object, e As EventArgs) Handles Button10.Click
        Dim a(2, 2), b(2, 2), c(2, 2)
        FileOpen(5, "C:\Users\dora0\Desktop\NTVS\丙級\軟設丙\測資\5.txt", OpenMode.Input)
        For i = 1 To 2
            For j = 1 To 2
                Input(5, a(i, j))
            Next
        Next
        For i = 1 To 2
            For j = 1 To 2
                Input(5, b(i, j))
            Next
        Next
        For i = 1 To 2
            For j = 1 To 2
                c(i, j) = a(i, j) + b(1, j)
            Next
        Next
        FileClose(5)
        TextBox10.Text = "第五題結果為 : " & vbNewLine & "[" & c(1, 1) & "    " & c(1, 2) & "]" & vbNewLine & "[" & c(2, 1) & "    " & c(2, 2) & "]"
    End Sub
End Class