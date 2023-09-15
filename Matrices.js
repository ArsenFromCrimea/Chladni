function TransMatrix(A)       //�� ����� ��������� ������
{
    var m = A.length, n = A[0].length, AT = [];
    for (var i = 0; i < n; i++)
     { AT[i] = [];
       for (var j = 0; j < m; j++) AT[i][j] = A[j][i];
     }
    return AT;
}


function SumMatrix(A,B)       //�� ����� ��������� ������� ���������� �����������
{   
    var m = A.length, n = A[0].length, C = [];
    for (var i = 0; i < m; i++)
     { C[i] = [];
       for (var j = 0; j < n; j++) C[i][j] = A[i][j]+B[i][j];
     }
    return C;
}

function minusMatrix(A,B){
	var C=multMatrixNumber(-1,B);
	return SumMatrix(A,C);
}


function multMatrixNumber(a,A)  // a - �����, A - ������� (��������� ������)
{   
    var m = A.length, n = A[0].length, B = [];
    for (var i = 0; i < m; i++)
     { B[i] = [];
       for (var j = 0; j < n; j++) B[i][j] = a*A[i][j];
     }
    return B;
}

function MultiplyMatrix(A,B) // ������������ ������
{
    var rowsA = A.length, colsA = A[0].length,
        rowsB = B.length, colsB = B[0].length,
        C = new Array(rowsA);
    if (colsA != rowsB) return false;
    for (var i = 0; i < rowsA; i++) C[i] = [];
    for (var k = 0; k < colsB; k++)
     { for (var i = 0; i < rowsA; i++)
        { var t = 0;
          for (var j = 0; j < rowsB; j++) t += A[i][j]*B[j][k];
          C[i][k] = t;
        }
     }
    return C;
}



function MatrixPow(n,A)
{ 
    if (n == 1) return A;     // ������� MultiplyMatrix ��. ����
    else return MultiplyMatrix( A, MatrixPow(n-1,A) );
}


function Determinant(A)   // ������������ �������� �������, ��������� O(n^3)
{
    var N = A.length, B = [], denom = 1, exchanges = 0;
    for (var i = 0; i < N; ++i)
     { B[i] = [];
       for (var j = 0; j < N; ++j) B[i][j] = A[i][j];
     }
    for (var i = 0; i < N-1; ++i)
     { var maxN = i, maxValue = Math.abs(B[i][i]);
       for (var j = i+1; j < N; ++j)
        { var value = Math.abs(B[j][i]);
          if (value > maxValue){ maxN = j; maxValue = value; }
        }
       if (maxN > i)
        { var temp = B[i]; B[i] = B[maxN]; B[maxN] = temp;
          ++exchanges;
        }
       else { if (maxValue == 0) return maxValue; }
       var value1 = B[i][i];
       for (var j = i+1; j < N; ++j)
        { var value2 = B[j][i];
          B[j][i] = 0;
          for (var k = i+1; k < N; ++k) B[j][k] = (B[j][k]*value1-B[i][k]*value2)/denom;
        }
       denom = value1;
     }
    if (exchanges%2) return -B[N-1][N-1];
    else return B[N-1][N-1];
}



function MatrixRank(A)
{
    var m = A.length, n = A[0].length, k = (m < n ? m : n), r = 1, rank = 0;
    while (r <= k)
     { var B = [];
       for (var i = 0; i < r; i++) B[i] = [];
       for (var a = 0; a < m-r+1; a++)
        { for (var b = 0; b < n-r+1; b++)
           { for (var c = 0; c < r; c++)
              { for (var d = 0; d < r; d++) B[c][d] = A[a+c][b+d]; }
             if (Determinant(B) != 0) rank = r;
           }       // ������� Determinant ��. ����
        }
       r++;
     }
    return rank;
}


function AdjugateMatrix(A)   // A - ��������� ���������� ������
{                                        
    var N = A.length, adjA = [];
    for (var i = 0; i < N; i++)
     { adjA[i] = [];
       for (var j = 0; j < N; j++)
        { var B = [], sign = ((i+j)%2==0) ? 1 : -1;
          for (var m = 0; m < j; m++)
           { B[m] = [];
             for (var n = 0; n < i; n++)   B[m][n] = A[m][n];
             for (var n = i+1; n < N; n++) B[m][n-1] = A[m][n];
           }
          for (var m = j+1; m < N; m++)
           { B[m-1] = [];
             for (var n = 0; n < i; n++)   B[m-1][n] = A[m][n];
             for (var n = i+1; n < N; n++) B[m-1][n-1] = A[m][n];
           }
          adjA[i][j] = sign*Determinant(B);   // ������� Determinant ��. ����
        }
     }
    return adjA;
}


function InverseMatrix(A)   // A - ��������� ���������� ������
{   
    var det = Determinant(A);                // ������� Determinant ��. ����
    if (det == 0) return false;
    var N = A.length, A = AdjugateMatrix(A); // ������� AdjugateMatrix ��. ����
    for (var i = 0; i < N; i++)
     { for (var j = 0; j < N; j++) A[i][j] /= det; }
    return A;
}


function ZeroMatrix(m,n) // ������� �������
{
	var result=new Array(m);
	for(var i=0;i<m;i++){
		result[i]=new Array(n);
		for(var j=0;j<n;j++){
			result[i][j]=0;
		}
	}
	return result;
}


function RandomMatrix(m,n) // ������� �������
{
	var result=new Array(m);
	for(var i=0;i<m;i++){
		result[i]=new Array(n);
		for(var j=0;j<n;j++){
			result[i][j]=Math.random()*10;
		}
	}
	return result;
}




function IdentityMatrix(m) //  ��������� �������
{
	var result=new Array(m);
	for(var i=0;i<m;i++){
		result[i]=new Array(m);
		for(var j=0;j<m;j++){
			result[i][j]=(i==j?1:0);
		}
	}
	return result;
}



function Norm(A) //  �����
{
	var result=0;
	for(var i=0;i<A.length;i++){
		for(var j=0;j<A[i].length;j++){
			result=result+Math.pow(A[i][j],2);
		}
	}
	return Math.sqrt(result);
}


function ZeroVector(m){
	return ZeroMatrix(m,1);
}

function Vector(a){ // ������ ��������������� ������� a �������
	var result=new Array(a.length);
	for(var i=0;i<a.length;i++){
		result[i]=[a[i]];
	}
	return result;
}


function Print(A)
{
	var result="";
	for(var i=0;i<A.length;i++){
		var string="";
		for(var j=0;j<A[i].length;j++){
			string=string+"<td>"+A[i][j]+"</td>";
		}
		result=result+"<tr>"+string+"</tr>";
	}
	return "<table border='1'>"+result+"</table>";
}
