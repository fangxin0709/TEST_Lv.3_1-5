Public Class Form1
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim X
        X = Val(TextBox1.Text)
        If X = "1234" Then
            TextBox2.Text = "login"
        Else
            TextBox2.Text = "err"
        End If



    End Sub
    '檢定for
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim data
        FileOpen(2, "C:\Users\dora0\Desktop\2.txt", OpenMode.Input)
        Input(2, data)
        FileClose(2)
        For X = 1 To data
            For Z = 1 To X
                TextBox3.Text = TextBox3.Text & Z
            Next
            TextBox3.Text = TextBox3.Text & vbNewLine
        Next
    End Sub
    '檢定反向
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim data
        FileOpen(22, "C:\Users\dora0\Desktop\2.txt", OpenMode.Input)
        Input(22, data)
        FileClose(22)
        TextBox2.Clear()
        For X = data To 1 Step -1
            For Z = 1 To X
                TextBox2.Text = TextBox2.Text & Z
            Next
            TextBox2.Text = TextBox2.Text & vbNewLine
        Next

    End Sub
    '檢定do while
    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim data
        FileOpen(2, "C:\Users\dora0\Desktop\2.txt", OpenMode.Input)
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
    '檢定loop while
    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Dim Data
        FileOpen(2, "C:\Users\dora0\Desktop\2.txt", OpenMode.Input)
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
    '檢定菜有文
    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        Dim all, lla, m1, n2, go
        '原始資料 反轉資料 商數 餘數 運算資料
        FileOpen(1, "C:\Users\dora0\Desktop\1.txt", OpenMode.Input)
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
    '檢定質數
    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        Dim data, num
        FileOpen(3, "C:\Users\dora0\Desktop\3.txt", OpenMode.Input)
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
        FileOpen(4, "C:\Users\dora0\Desktop\4.txt", OpenMode.Input)
        TextBox8.Clear()
        For I = 1 To 3
            Input(4, H(I))
            Input(4, W(I))
            BMI(I) = W(I) / (H(I) / 100) ^ 2
        Next
        FileClose(4)
        Dim x, y, z
        For y = 1 To 2
            For x = 1 To 2
                If BMI(x) > BMI(x + 1) Then
                    z = BMI(x)
                    BMI(x) = BMI(x + 1)
                    BMI(x + 1) = z
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
        FileOpen(5, "C:\Users\dora0\Desktop\5.txt", OpenMode.Input)
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
    '二微陣列
    Private Sub Button10_Click(sender As Object, e As EventArgs) Handles Button10.Click
        Dim a(2, 2), b(2, 2), c(2, 2)
        FileOpen(5, "C:\Users\dora0\Desktop\5.txt", OpenMode.Input)
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