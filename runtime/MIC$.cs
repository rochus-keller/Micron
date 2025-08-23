
using System.Runtime.InteropServices;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.IO;
using System;

public class MIC_Dollar
{
    static Dictionary<string, IntPtr> dict = new Dictionary<string, IntPtr>();
    static System.Text.Encoding latin1 = System.Text.Encoding.GetEncoding("ISO-8859-1");

    public static IntPtr strlit(string str)
    {
        IntPtr res;
        if( dict.TryGetValue(str, out res) )
            return res;
        else
        {
            byte[] bytes = latin1.GetBytes(str); 
            res = Marshal.AllocHGlobal(bytes.Length+1);
            Marshal.Copy(bytes, 0, res, bytes.Length);
            Marshal.WriteByte(res, bytes.Length, 0);
            dict[str] = res;
            return res;
        }
    }
    
	public static unsafe int strcmp( byte* lhs, byte* rhs )
	{
	    if (lhs == null && rhs == null) return 0;
        if (lhs == null) return -1;
        if (rhs == null) return 1;

	    // source: https://stackoverflow.com/questions/34873209/implementation-of-strcmp/34873406
		for (int i = 0; ; i++)
		{
			if( lhs[i] != rhs[i] )
			    return lhs[i] < rhs[i] ? -1 : 1;

			if( lhs[i] == 0 )
			    return 0;
		}
    }  
	      
    public static unsafe bool relop1(byte* l, byte* r, int op)
    {
	    switch( op )
	    {
	    case 1: // EQ
		    return strcmp(l,r) == 0;
	    case 2: // NEQ
		    return strcmp(l,r) != 0;
	    case 3: // LT
		    return strcmp(l,r) < 0;
	    case 4: // LEQ
		    return strcmp(l,r) <= 0;
	    case 5: // GT
		    return strcmp(l,r) > 0;
	    case 6: // GEQ
		    return strcmp(l,r) >= 0;
	    }
	    return false;
    }

    public static unsafe bool relop2(byte* lhs, byte rhs, int op)
    {
        byte* ch = stackalloc byte[2];
	    ch[0] = rhs;
	    ch[1] = 0;
	    return relop1(lhs,ch,op);
    }

    public static unsafe bool relop3(byte lhs, byte* rhs, int op)
    {
        byte* ch = stackalloc byte[2];
	    ch[0] = lhs;
	    ch[1] = 0;
	    return relop1(ch,rhs,op);
    }

    public static unsafe bool relop4(byte lhs, byte rhs, int op)
    {
        byte* l = stackalloc byte[2];
        byte* r = stackalloc byte[2];
	    l[0] = lhs;
	    l[1] = 0;
	    r[0] = rhs;
	    r[1] = 0;
	    return relop1(l,r,op);
    }

    public static uint SetDiv( uint lhs, uint rhs )
    {
        return ~( lhs & rhs ) & ( lhs | rhs );
    }

    public static bool SetIn( uint lhs, uint rhs )
    {
        return ((1<<(int)lhs)&rhs) != 0;
    }

    public static void printI8(long i)
    {
        System.Console.Write(i);
    }

    public static void printU8(ulong i)
    {
        System.Console.Write(i);
    }

    public static void printF8(double d)
    {
	    System.Console.Write(d);
    }

    public static unsafe void printStr(byte* s)
    {
        if( s == null ) return;
        int length = 0;
        while (s[length] != 0)
            length++;    
        System.Console.Write(latin1.GetString(s, length));
    }

    public static void printCh(byte c)
    {
	    System.Console.Write((char)c);
    }

    public static void printBool(bool b)
    {
	    if( b )
	        System.Console.Write("true");
	    else
	        System.Console.Write("false");
    }

    public static void printSet(uint s)
    {
        System.Console.Write(Convert.ToString(s, 2).PadLeft(32, '0'));
    }

    public static unsafe void assert(byte cond, uint line, byte* file)
    {
        if(cond == 0 && file != null)
        {
            int length = 0;
            while (file[length] != 0)
                length++;    
            Console.Error.WriteLine("assertion failed in {0} line {1}", latin1.GetString(file, length), line);
        }
	    System.Diagnostics.Debug.Assert(cond != 0);
    }

    public static void exit(int res)
    {
        // NOP
    }
}
