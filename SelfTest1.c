#pragma token off

int main()
{
    int a = 2000;

    if(a > 100)
    {
        digitalWrite(13, HIGH);
        delay(a);
        digitalWrite(13, LOW);
        delay(a);
    }

    return 0;
}
