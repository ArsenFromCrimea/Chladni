a=function(vertical){
	return (vertical?1:1);
}

nu=0.225;

h=function(m,vertical){
	return m*Math.PI/a(vertical);
}

h2=function(m,vertical){
	return Math.pow(h(m,vertical),2);
}



q_2=function(m,k,vertical,i){
	
	return h2(m,vertical)+(i==1?-1:1)*Math.pow(k,2);
}


C=function(m,n,k,vertical){
	var up=2*Math.pow(k,2)*(nu*Math.pow(k,4)+Math.pow((nu-1),2)*h2(n,!vertical)*h2(m,vertical));
	var down=(q_2(n,k,!vertical,2)-(2-nu)*h2(n,!vertical))*(q_2(n,k,!vertical,1)+h2(m,vertical))*(q_2(n,k,!vertical,2)+h2(m,vertical));
	return up/down*(m==0?1:2);
}

/************************************Пришлось поморочить голову (какая-то недокомплексность)*************************************/
function sinh(x){	//Почему-то отсутствовали
    return (Math.exp(x) - Math.exp(-x)) / 2;
}


function cosh(x){
    return (Math.exp(x) + Math.exp(-x)) / 2;
}



ch_=function(m,k,vertical,i,x){
	var qq=q_2(m,k,vertical,i);
	return (qq>=0?cosh(Math.sqrt(qq)*x):Math.cos(Math.sqrt(-qq)*x));
}

ch=function(m,k,vertical,i){
	return ch_(m,k,vertical,i,a(!vertical));
}


qsh=function(m,k,vertical,i){
	var qq=q_2(m,k,vertical,i);
	var q=Math.sqrt(Math.abs(qq));
	return q*(qq>=0?sinh(q*a(!vertical)):-Math.sin(q*a(!vertical)));
}
/***********************************************************************************************************************************************/


delta_=function(m,k,vertical,i){
	return a(!vertical)*(q_2(m,k,vertical,3-i)-(2-nu)*h2(m,vertical))/qsh(m,k,vertical,i)*(q_2(m,k,vertical,i)-nu*h2(m,vertical))*ch(m,k,vertical,i)
}



delta=function(m,k,vertical){
	return delta_(m,k,vertical,1)-delta_(m,k,vertical,2);
}



Matrix=function(m,n,k,vertical){
	return C(m,n,k,vertical)*qsh(n,k,!vertical,1)/a(vertical)*(m%2==0?1:-1)*(n%2==0?1:-1);
}

Delta=function(m,k,vertical){
	return qsh(m,k,vertical,1)/a(!vertical)*delta(m,k,vertical)/(q_2(m,k,vertical,2)-(2-nu)*h2(m,vertical));
}



exceptionHarmonic=0;
exceptionHorisontal=true;

resonance_right_part=function(k,vertical){
	var result=new Array(size);
	var i;
	if(exceptionHorisontal!=vertical){
		for(i=0;i<size;i++){
			result[i]=[0];
		}
	}
	else{
		for(i=0;i<size;i++){
			result[i]=[Matrix(i,exceptionHarmonic,k,vertical)];	
		}
	}
	return result;
}

finite_matrix=function(k,vertical){
	var result=new Array(size);
	for(var i=0;i<size;i++){
		result[i]=new Array(size);
	}
	var count=0;
	for(i=0;count<size;i++){
			if(i!=exceptionHarmonic){
				for(var j=0;j<size;j++){
					if(exceptionHorisontal==vertical){
						result[j][count]=Matrix(j,i,k,vertical);
					}
					else{
						result[count][j]=Matrix(i,j,k,vertical);
					}
				}
				count++;
			}
	}
	
	return result;
}

finite_delta=function(k,vertical){
	var result=new Array(size);
	var i;
	if(exceptionHorisontal==vertical){
		for(i=0;i<size;i++){
			result[i]=Delta(i,k,vertical);
		}
	}
	else{
		var count=0;
		for(i=0;count<size;i++){
			if(i!=exceptionHarmonic){
				result[count]=Delta(i,k,vertical);
				count++;
			}
		}
	}
	return result;

}

resonance_solve=function(k,vertical){
	var delta=function(vertical,back){
		var result=new Array(size);
		var my_delta=finite_delta(k,vertical);
		for(var i=0;i<size;i++){
			result[i]=new Array(size);
			for(var j=0;j<size;j++){
				if(back){
					result[i][j]=(i==j?1/my_delta[i]:0);
				}
				else{
					result[i][j]=(i==j?my_delta[i]:0);
				}
			}
		}
		return result;
	}
	var matrix=minusMatrix(delta(vertical,false),MultiplyMatrix(finite_matrix(k,vertical),MultiplyMatrix(delta(!vertical,true),finite_matrix(k,!vertical))));
	var right=SumMatrix(MultiplyMatrix(finite_matrix(k,vertical),MultiplyMatrix(delta(!vertical,true),resonance_right_part(k,!vertical))),resonance_right_part(k,vertical));
	return MultiplyMatrix(InverseMatrix(matrix),right);	
}


/**********************************************************************************************************************/

size=8;





//Пусть имеется массив aVertical и массив aHorisontal


eta=function(m,k,vertical){
	return qsh(m,k,vertical,1)/qsh(m,k,vertical,2);
}

xi=function(m,k,vertical){
	return -(q_2(m,k,vertical,1)-(2-nu)*h2(m,vertical))/(q_2(m,k,vertical,2)-(2-nu)*h2(m,vertical))*eta(m,k,vertical);
}


w=function(x,y,a,k,vertical){
	var phi=0;
	var psi=0;
	for(var m=0;m<a.length;m++){
		phi=phi+a[m][0]*ch_(m,k,vertical,1,vertical?x:y)*Math.cos(h(m,vertical)*(vertical?y:x));
		psi=psi+a[m][0]*xi(m,k,vertical)*ch_(m,k,vertical,2,vertical?x:y)*Math.cos(h(m,vertical)*(vertical?y:x));
	}
	return phi+psi;
}









changeScreen2=function(k,pause,x0,y0,a_,b_){
	var size=pause?19:a_;
	var grid=function(){
		canvas.beginPath();
		canvas.strokeStyle="#00ff00";
		for(var i=-size;i<=size;i++){
			canvas.moveTo(x0-a_,y0+i*square);
			canvas.lineTo(x0+a_,y0+i*square);
			canvas.stroke();
		}
		for(var j=-size;j<=size;j++){
			canvas.moveTo(x0+j*square,y0-b_);
			canvas.lineTo(x0+j*square,y0+b_);
			canvas.stroke();
		}

	}

	var axes=function(){
		canvas.beginPath();
		canvas.strokeStyle="#ff0000";
		canvas.moveTo(0,y0);
		canvas.lineTo(screen2.width,y0);
		canvas.stroke();
		canvas.moveTo(x0,0);
		canvas.lineTo(x0,screen2.height);
		canvas.stroke();
		canvas.closePath();
	}

	if(window.generator){
		clearInterval(generator);
	}
	square=a_/size;
	
	var vertical_=resonance_solve(k,true);
	var horisontal_=resonance_solve(k,false);
	var add=function(vector){
		var result=new Array(vector.length+1);
		var count=0;
		for(var i=0;i<result.length;i++){
			if(i==exceptionHarmonic){
				result[i]=[1];
			}
			else{
				result[i]=vector[count];
				count++;
			}
		}
		return result;
	}

	if(exceptionHorisontal){
		solution={vertical:vertical_,horisontal:add(horisontal_)};
	}
	else{
		solution={vertical:add(vertical_),horisontal:horisontal_};
	}
	
	screen2=document.getElementById("myScreen2");
	canvas=screen2.getContext("2d");
	canvas.lineWidth=1;
	canvas.clearRect(0,0,screen2.width,screen2.height);
	axes();

	if(pause){
		grid();
	}


	
	
	var putPixel=function(xx,yy){
		if(pause){
			canvas.fillRect(x0+xx*square+1,y0+yy*square+1,square-2,square-2);
		}
		else{
			canvas.fillRect(x0+xx,y0+yy,1,1);
		}

	}
	
	var onePixel=function(xx,yy){
		putPixel(-xx-1,-yy-1);
		putPixel(xx,-yy-1);
		putPixel(xx,yy);
		putPixel(-xx-1,yy);
	}
	
	var i=0;
	var j=0;
	var paintMe=function(i,j){
		var x=j*a(false)/size;
		var y=i*a(true)/size;
		canvas.fillStyle=((w(x,y,solution.vertical,k,true)+w(x,y,solution.horisontal,k,false)<0)?"#0000ff":"#ffff00");

		onePixel(j,i);
	}

	var onePaint=function(){
			paintMe(i,j);	
			if(j<size-1){
				j++;
			}
			else{
				j=0;
				if(i<size-1){
					i++;
				}
				else{
					clearInterval(generator);
				}
			}
	}

	if(pause){
		generator=setInterval(onePaint,5);
	}
	else{
		for(i=0;i<b_;i++){
			for(j=0;j<a_;j++){
				paintMe(i,j);
			}
		}
		axes();
	}
	return find_error();
}



find_error=function(){
	var sum=-1;
	for(j=0;j<size;j++){
		var one=Matrix(exceptionHarmonic,j,k,!exceptionHorisontal)/Delta(exceptionHarmonic,k,!exceptionHorisontal)*(exceptionHorisontal?solution.vertical[j]:solution.horisontal[j]);
		sum=sum+one;
	}	
	return sum;
}