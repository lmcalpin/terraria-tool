package com.metatrope.util
import java.io.FileWriter

trait IO {
    def printToFile(fileName: String)(op: java.io.PrintWriter => Unit) {
        using (new java.io.FileWriter(fileName)) { fw =>
            using (new java.io.PrintWriter(fw)) { pw =>
            	op(pw)
            }
        }
    }

    def using[A, B <: { def close(): Unit }](closeme: B)(f: B => A): A = {
        try { 
            f(closeme) 
        } finally { 
            closeme.close()
        }
    }

}