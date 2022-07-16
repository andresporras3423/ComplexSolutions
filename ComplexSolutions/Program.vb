Imports System

Module Program

    Sub BubbleAlgorithm(list As List(Of Int32))
        Dim i = 0
        While i < list.Count
            Dim j = 1
            While j < list.Count - i
                If list(j) < list(j - 1) Then
                    Dim temp = list(j)
                    list(j) = list(j - 1)
                    list(j - 1) = temp
                End If
                j += 1
            End While
            i += 1
        End While
        Console.WriteLine(String.Join(",", list))
    End Sub

    Sub SieveOfErasthotenes(max As Int32)
        Dim nums = Enumerable.Range(2, max - 2).ToHashSet
        Dim i = 2
        While i <= max
            If (nums.Contains(i)) Then
                Dim j = i * 2
                While j <= max
                    nums.Remove(j)
                    j += i
                End While
            End If
            i += 1
        End While
        Console.WriteLine(String.Join(",", nums))
    End Sub

    Sub InsertionAlgorithm(list As List(Of Int32))
        For Each i In Enumerable.Range(1, list.Count - 1)
            Dim last = i
            While last > 0
                If list(last) < list(last - 1) Then
                    Dim temp = list(last)
                    list(last) = list(last - 1)
                    list(last - 1) = temp
                Else
                    Exit While
                End If
                last -= 1
            End While
        Next
        Console.WriteLine(String.Join(",", list))
    End Sub

    Sub QuickSort(list As List(Of Int32))
        Dim part = Partition(list)
        Console.WriteLine(String.Join(",", TryCast(part, List(Of Int32))))
    End Sub

    Function Partition(list As List(Of Int32))
        If list.Count < 2 Then
            Return list
        End If
        Dim mid = list(0)
        Dim before = New List(Of Int32)
        Dim after = New List(Of Int32)
        list.RemoveAt(0)
        For Each num In list
            If num > mid Then
                after.Add(num)
            Else
                before.Add(num)
            End If
        Next
        before = Partition(before)
        after = Partition(after)
        before.AddRange(New List(Of Int32) From {mid})
        before.AddRange(after)
        Return before
    End Function

    Sub MergeSort(list As List(Of Int32))
        Dim part = Merge(list)
        Console.WriteLine(String.Join(",", TryCast(part, List(Of Int32))))
    End Sub

    Function Merge(list As List(Of Int32))
        If list.Count < 2 Then
            Return list
        End If
        Dim limit1 = Math.Floor(list.Count / 2)
        Dim list1 = list.GetRange(0, limit1)
        Dim list2 = list.GetRange(limit1, list.Count - limit1)
        list1 = Merge(list1)
        list2 = Merge(list2)
        Dim sol = New List(Of Int32)
        For Each i In Enumerable.Range(0, list.Count)
            If (list2.Count > 0 AndAlso (list1.Count = 0 OrElse (list2(0) < list1(0)))) Then
                sol.Add(list2(0))
                list2.RemoveAt(0)
            Else
                sol.Add(list1(0))
                list1.RemoveAt(0)
            End If
        Next
        Return sol
    End Function

    Sub CountSort(list As List(Of Int32))
        Dim max = list.Max
        Dim nums(max) As Int32
        For Each item In list
            nums(item) = item
        Next
        Dim sol = nums.Where(Function(n, i) n > 0)
        Console.WriteLine(String.Join(",", sol))
    End Sub

    'efficient method to get x^n
    Function Pow(x As Double, n As Double)
        If n < 0 Then
            Return Pow(1 / x, -1 * n)
        ElseIf n = 0 Then
            Return 1
        ElseIf n = 1 Then
            Return x
        ElseIf n Mod 2 = 1 Then
            Return x * Pow(x * x, (n - 1) / 2)
        End If
        Return Pow(x * x, n / 2)
    End Function

    '    efficient method To Get a close aproximation To the square Of a number In few steps
    Function Sqrt(x As Double)
        Dim string_x = x.ToString
        Dim first_middle = string_x.Substring(0, Math.Floor(string_x.Length / 2))
        Dim trunc = Convert.ToDouble(first_middle) + 1
        While True
            trunc = ((x / trunc) + trunc) / 2
            If (trunc + 0.0001) ^ 2 > x And trunc ^ 2 <= x Then
                Return trunc
            End If
        End While
    End Function

    'Get determinant Of a matrix by laplace
    Function LaplaceDeterminant(matrix As Int32()())
        If matrix.Any(Function(x) x.Length <> matrix.Length) Then
            Return "matrix must be nxn"
        End If
        Return MatrixRecursion(matrix)
    End Function

    Function MatrixRecursion(matrix As Int32()())
        If matrix.Length = 2 Then
            Return (matrix(0)(0) * matrix(1)(1)) - (matrix(0)(1) * matrix(1)(0))
        End If
        Dim sol = 0
        For Each i In Enumerable.Range(0, matrix(0).Length)
            Dim subMatrix = matrix.Skip(1).Select(Function(m) m.ToList).ToArray
            For Each row As List(Of Int32) In subMatrix
                row.RemoveAt(i)
            Next
            Dim nSubMatrix = subMatrix.Select(Function(x) x.ToArray).ToArray
            sol += ((-1) ^ i) * matrix(0)(i) * MatrixRecursion(nSubMatrix)
        Next
        Return sol
    End Function


    'depth first search algorithm for a graph tree
    Function FirstSearch(graph)
        Dim g1 = New GraphList(graph)
        Return g1.GetList
    End Function
    Class GraphList
        Dim graph As Dictionary(Of Int32, Int32())
        Dim word As List(Of Int32)
        Public Sub New(nGraph)
            graph = nGraph
        End Sub
        Function GetList()
            word = New List(Of Integer)
            ItemNext(0)
            Return String.Join(",", word)
        End Function

        Sub ItemNext(item)
            If word.Contains(item) Then
                Return
            End If
            word.Add(item)
            For Each num In graph(item)
                ItemNext(num)
            Next
        End Sub
    End Class

    'breath first search algorithm For a graph tree
    Function bfs(graph)
        Dim sol = New List(Of Int32) From {0}
        nextLevel(New List(Of Int32) From {0}, sol, graph)
        Return String.Join(",", sol)
    End Function

    Sub nextLevel(list As List(Of Int32), sol As List(Of Int32), graph As Dictionary(Of Int32, Int32()))
        Dim newList = New List(Of Int32)
        For Each item In list
            For Each child In graph(item)
                If Not sol.Contains(child) Then
                    sol.Add(child)
                    newList.Add(child)
                End If
            Next
        Next
        If newList.Count = 0 Then
            Return
        End If
        nextLevel(newList, sol, graph)
    End Sub

    Class Node
        Public val As String
        Public left As Node
        Public right As Node
        Public Sub New(nVal)
            val = nVal
        End Sub
    End Class

    'pre-order: root, left, right
    Function PreOrder(node As Node)
        If node Is Nothing OrElse node.val Is Nothing Then
            Return ""
        End If
        Dim result = node.val + " "
        result += PreOrder(node.left)
        result += PreOrder(node.right)
        Return result
    End Function

    'in-order: left, root, right
    Function InOrder(node As Node)
        If node Is Nothing OrElse node.val Is Nothing Then
            Return ""
        End If
        Dim result = InOrder(node.left)
        result += node.val + " "
        result += InOrder(node.right)
        Return result
    End Function

    'post-order: left, right, root
    Function PostOrder(node As Node)
        If node Is Nothing OrElse node.val Is Nothing Then
            Return ""
        End If
        Dim result = PostOrder(node.left)
        result += PostOrder(node.right)
        result += node.val + " "
        Return result
    End Function

    'show steps To move hanoi discs from 1 To 3 With <number_of_discs> numbers Of discs
    Sub HanoiSteps(number_of_discs)
        Tower(number_of_discs, "1", "2", "3")
    End Sub

    Sub Tower(n, from_rod, aux_rod, to_rod)
        If n = 1 Then
            Console.WriteLine(from_rod + "->" + to_rod)
        Else
            Tower(n - 1, from_rod, to_rod, aux_rod)
            Console.WriteLine(from_rod + "->" + to_rod)
            Tower(n - 1, aux_rod, from_rod, to_rod)
        End If
    End Sub

    'find minimum number of items to delete to avoid overlap of intervals
    Function eraseOverlapIntervals(intervals As List(Of Int32()))
        Dim sorted = MergeSortPairs(intervals)
        Return Overlap(sorted)
    End Function

    Function MergeSortPairs(intervals As List(Of Int32()))
        Dim list1 = intervals.GetRange(0, Math.Floor(intervals.Count / 2))
        Dim list2 = intervals.GetRange(Math.Floor(intervals.Count / 2), intervals.Count - Math.Floor(intervals.Count / 2))
        If list1.Count > 1 Then
            list1 = MergeSortPairs(list1)
        End If
        If list2.Count > 1 Then
            list2 = MergeSortPairs(list2)
        End If
        Dim sol As List(Of Int32()) = New List(Of Int32())
        For Each i In Enumerable.Range(0, intervals.Count)
            If list2.Count > 0 AndAlso (list1.Count = 0 OrElse list2(0)(0) < list1(0)(0)) Then
                sol.Add(list2(0).Clone)
                list2.RemoveAt(0)
            Else
                sol.Add(list1(0).Clone)
                list1.RemoveAt(0)
            End If
        Next
        Return sol
    End Function

    Function Overlap(sorted As List(Of Int32()))
        Dim first = 0
        Dim second = 1
        Dim sol = 0
        While second < sorted.Count
            If sorted(first)(1) > sorted(second)(0) Then
                sol += 1
                If sorted(first)(1) > sorted(second)(1) Then
                    first = second
                End If
            Else
                first = second
            End If
            second += 1
        End While
        Return sol
    End Function


    Sub Main(args As String())
        'BubbleAlgorithm(New List(Of Integer) From {2, 1, 6, 7, 4})
        'SieveOfErasthotenes(100)
        'InsertionAlgorithm(New List(Of Integer) From {2, 1, 6, 7, 4, 10, 0})
        'QuickSort(New List(Of Integer) From {2, 1, 6, 7, 4, 10, 0})
        'MergeSort(New List(Of Integer) From {2, 1, 6, 7, 4, 10, 0})
        'CountSort(New List(Of Integer) From {2, 1, 6, 7, 4, 10, 0})
        'Console.WriteLine(Pow(2, -4))
        'Console.WriteLine(Sqrt(16))
        'Console.WriteLine(LaplaceDeterminant(New Int32()() {New Int32() {2, -3, 9}, New Int32() {2, 0, -1}, New Int32() {1, 4, 5}}))
        'Console.WriteLine(FirstSearch(New Dictionary(Of Int32, Int32()) From {
        '    {0, {2}},
        '    {1, {4}},
        '    {2, {5, 0, 3}},
        '    {3, {2}},
        '    {4, {1, 5}},
        '    {5, {4, 2}}
        '                              }))
        'Console.WriteLine(bfs(New Dictionary(Of Int32, Int32()) From {
        '    {0, {2}},
        '    {1, {4}},
        '    {2, {5, 0, 3}},
        '    {3, {2}},
        '    {4, {1, 5}},
        '    {5, {4, 2}}
        '                              }))

        'nextLevel line to test pre order
        'Dim node1 = New Node("1")
        'node1.left = New Node("2")
        'node1.right = New Node("3")
        'node1.left.left = New Node("5")
        'node1.left.left.left = New Node("6")
        'node1.right.right = New Node("4")
        'node1.right.left = New Node("7")
        'Console.WriteLine(PreOrder(node1))

        'nextLevel line to test in order
        'Dim node1 = New Node("1")
        'node1.left = New Node("2")
        'node1.right = New Node("3")
        'node1.left.left = New Node("5")
        'node1.left.left.left = New Node("6")
        'node1.right.right = New Node("4")
        'node1.right.left = New Node("7")
        'Console.WriteLine(InOrder(node1))

        'nextLevel line to test post order
        'Dim node1 = New Node("1")
        'node1.left = New Node("2")
        'node1.right = New Node("3")
        'node1.left.left = New Node("5")
        'node1.left.left.left = New Node("6")
        'node1.right.right = New Node("4")
        'node1.right.left = New Node("7")
        'Console.WriteLine(PostOrder(node1))

        'HanoiSteps(3)

        'Console.WriteLine(eraseOverlapIntervals(New List(Of Integer()) From {{{1, 2}}, {{7, 9}}, {{3, 5}}, {{2, 10}}, {{2, 4}}}))
    End Sub
End Module