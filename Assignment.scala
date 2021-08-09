

object Assignment {
  
    // 97 to 123 alphabet in ascii tables

    // Encryption Function
    def encription(Text: String, shifts: Int): String = {
        return Text.map(msg => {
            if('a'.toInt to 'z'.toInt contains msg.toInt) {
                (((msg.toInt - 97)+shifts) % 26)match {
                    case x if(x<0) => (x+123).toChar 
                    case x if(x>=0) => (x+97).toChar  
                }
            }else msg
        })
    }

    // 97 to 123 alphabet in ascii tables
    
    // Decryption Function
    def decription(encryptedText: String, shifts: Int): String = {
        return encryptedText.map(msg => {
            if('a'.toInt to 'z'.toInt contains msg.toInt) {
                (((msg.toInt - 97)-shifts) % 26)match {
                    case x if(x<0) => (x+123).toChar 
                    case x if(x>=0) => (x+97).toChar  
                }
            }else msg
        })
    }

    // cipher function that takes in both encryption and decryption
    def cipher(string: String, shifts: Int, method: Char): String = method match {
        case 'E' => encription(string, shifts)
        case 'D' => decription(string, shifts)
    }
    
    // Main function
    def main(args: Array[String]): Unit = {
        println("________________________________________________\n");
        println("        	Ceasar Cipher :");
        println("________________________________________________\n");
        println(" For encription enter : E");
        println(" For decription enter : D");
        print("                Input : ");
        var method: Char = readChar();
        method match {
            case 'E' => {
                print("\n              Message : "); 
                var message: String = readLine();
                print("               Shifts : ");
                var shifts: Int = readInt(); 
                println("    Encripted message : "+ encription(message, shifts)); 
            }
            case 'D' => {
                print("\n              Message : "); 
                var message: String = readLine();
                print("               Shifts : ");
                var shifts: Int = readInt(); 
                println("    Decripted message : "+ decription(message, shifts));
            }
            case _ => {
                println("\n                Input is Invalid");
            }
        }
    }
}