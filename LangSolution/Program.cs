using System.Numerics;

class Program
{
    static void fibSolution()
    {
        BigInteger a = 0;
        BigInteger b = 1;
        int count = 1;
        while (b.ToString().Length < 1000)
        {
            count++;
            b = BigInteger.Add(a, b);
            a = BigInteger.Subtract(b, a);
        }

        Console.WriteLine(count);
    }

    static void sqrSolution()
    {
        int sum = 0;
        for (int i = 0; i <= 100; i++)
        {
            for (int j = 0; j < i; j++)
            {
                sum += j * i * 2;
            }
        }   
        Console.WriteLine(sum);
    }
    public static void Main(string[] args)
    {
        fibSolution();
        sqrSolution();
    }
}