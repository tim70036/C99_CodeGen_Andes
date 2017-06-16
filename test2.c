#pragma token off

int main()
{
    int a = 2000;

    digitalWrite(13, HIGH);
    delay(a);
    digitalWrite(13, LOW);
    delay(a);

    if (a != 0)
    {
        int b = 4000;
        int c = 2000;
        a = (b * 2 + c)/2;
        digitalWrite(13, HIGH);
        delay(a);
        digitalWrite(13, LOW);
        delay(a);
    }
    return 0;
}
