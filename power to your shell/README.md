


```batch
FOR /D %d IN (*) DO du -q "%d" | find "Size:"
```

requirements: SysInternals Suite on the path
