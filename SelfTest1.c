#pragma token off

int main()
{
    int a = 1000;
    int b = 1000;
    a = a - 0;
    digitalWrite(13, HIGH);
    delay(a);
    digitalWrite(13, LOW);
    delay(b);
    return 0;
}
