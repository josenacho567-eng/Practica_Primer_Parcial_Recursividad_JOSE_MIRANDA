import scala.annotation.tailrec

object Practica1Parcial{
    //EJERCICIO 1
    
    def decimalABinario(num: Int): String = {
        if num ==1 then "1"
        else {
            val aux: Int = num%2
            s"${decimalABinario(num/2)}$aux" 
        }

    }

    //EJERCICIO 2
    @tailrec
    def digitoMayor(num: Int, acum: Int = 0): Int = {
        if num <10 then {
            if num > acum then num
            else acum
        }
        else {
            if num%10 > acum then digitoMayor(num/10,num%10)
            else digitoMayor(num/10,acum)
        }
    }
    //EJERCICIO 3
    @tailrec
    def contarDigitosPares(num: Int, acum: Int=0):Int = {
        if num<10 then {
            if num == 0||num%2 == 0 then acum +1
            else acum
        }
        else {
            if num == 0 || num % 2 == 0 then 
                contarDigitosPares(num/10, acum+1)
            else contarDigitosPares(num/10,acum)
        }

    }

    //EJERCICIO 4
    @tailrec
    def contarDigito(num: Int, busqueda: Int, acum: Int = 0): Int = {
        if num<10 then {
            if num==busqueda then acum+1
            else acum
        }
        else{
            val numActual: Int = num%10
            if numActual == busqueda then contarDigito(num/10,busqueda,acum +1)
            else contarDigito(num/10,busqueda,acum)
        }
    }
    //EJERCICIO 5
    @tailrec
    def extraerPares(num: Int, acum: String = ""): String = {
        if num < 10 then {
            if num%2 == 0 then s"$num$acum"
            else acum
        }
        else {
            val numAux: Int = num%10
            if numAux%2==0 then extraerPares(num/10,s"$numAux$acum")
            else extraerPares(num/10, acum)
        }
    }

    //EJERCICIO 6
    def sumarDigitosPares(num: Int, acum: Int=0):  Int= {
        if num < 10 then {
            if num%2 == 0 then acum + num
            else acum
        } 
        else {
            val numAux: Int = num%10
            if numAux%2==0 then sumarDigitosPares(num/10,acum+numAux)
            else sumarDigitosPares(num/10, acum) 
        }
    }

    //EJERCICIO 7
    def contarMayuscula(pal: String, acum: Int=0): Int = {
        if pal== "" then acum
        else{
        if pal.length() ==1 then {
            if pal.head.isUpper then acum+1
            else acum
        }
        
        else {
            if pal.head.isUpper then contarMayuscula(pal.tail, acum+1)
            else contarMayuscula(pal.tail, acum) 
        }
        }
    }

    //EJERCICIO 8
    def contarCambiosParidad(num: Int, acum: Int=0): Int = {
        if num<10 then acum
        else {
            val numAux: Int = num%10
            val siguiente: Int = (num /10)%10
            if (numAux%2 ==0 && siguiente%2==0) || (numAux%2 !=0 && siguiente%2!=0) then contarCambiosParidad(num/10,acum)
            else contarCambiosParidad(num/10,acum+1)
            
        }

    }

    //EJERCICIO 9
    def segundoMayorDigito(num: Int,mayor:Int=0, acum: Int=0,iguales: Boolean = true): Int = {
        if num<10 then {
            if iguales then -1
            else{
                if num > mayor then mayor
                else {
                    if num > acum && num!=mayor then num
                    else acum
                }
            }
        }
        else {
            if num%10 > mayor then segundoMayorDigito(num/10,num%10, mayor, if mayor == 0 then iguales else false)
            else {
                if num%10> acum && num%10!=mayor then segundoMayorDigito(num/10,mayor,num%10,false)
                else if num % 10 == 0 then segundoMayorDigito(num/10,mayor, acum, false)
                else segundoMayorDigito(num/10,mayor, acum, iguales)
            }
        }
        
    }

    //EJERICICIO 10 
    def rachaMaxima(pal: String,anterior: Char = ' ' ,mayor: Int=0, actual: Int=0): Int = {
        if pal == "" then 0
        else{
        if pal.length() == 1 then {
            if anterior==pal.head then {
                if actual+1 > mayor then actual+1
                else mayor
            }
            else mayor
        }
        else {
            val auxSiguiente: Char = pal(1)
            if pal.head == auxSiguiente then { 
                rachaMaxima(pal.tail,pal.head,mayor,actual+1)
            }
            else {
                if actual+1>mayor then rachaMaxima(pal.tail,pal.head,actual+1,0)
                else rachaMaxima(pal.tail,pal.head,mayor,0)
            }
        }
        }
    }
    
    
    def main(args: Array[String]): Unit = {
        println(decimalABinario(4))
        println(digitoMayor(58927))
        println(contarDigitosPares(1357))
        println(contarDigito(707070,0))
        println(extraerPares(55044))
        println(sumarDigitosPares(8))
        println(contarMayuscula(""))
        println(contarCambiosParidad(11335))
        println(segundoMayorDigito(9))
        println(rachaMaxima("abccde"))

    }

    
}