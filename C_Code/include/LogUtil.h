#ifndef LOGUTIL_H
#define LOGUTIL_H

#define LOG_FILE_PATH "./tomo.log"

#define LOG_PROC "TomoProc"
#define LOG_NTWK "TomoNtwk"
#define LOG_IO   "TomoIO"
#define LOG_APPL "TomoAppl"
#define LOG_MISC "TomoMisc"

void verbose(const char * tag, const char * stringFormat, ...);
void debug(const char * tag, const char * stringFormat, ...);
void info (const char * tag, const char * stringFormat, ...);
void warn (const char * tag, const char * stringFormat, ...);
void error(const char * tag, const char * stringFormat, ...);

#endif
