#pragma token off

int main()
{
    int a = 2000;
    if (!a)
    {
        int b = 4000;
        int c = 2000;
        a = (b * 2 + c)/2;
        digitalWrite(13, HIGH);
        delay(a);
        digitalWrite(13, LOW);
        delay(a);
    }
    else
    {
        digitalWrite(13, HIGH);
        delay(a);
        digitalWrite(13, LOW);
        delay(a);
    }
    return 0;
}
