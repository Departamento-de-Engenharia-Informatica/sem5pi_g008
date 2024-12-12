using System.Security.Claims;
using Sempi5.Domain.Shared;
using Sempi5.Infrastructure.Shared;
using Microsoft.AspNetCore.Authentication.Cookies;
using Sempi5.Infrastructure.StaffRepository;
using Sempi5.Infrastructure.Databases;
using Sempi5.Bootstrappers;
using Sempi5.Domain;
using Sempi5.Domain.AgendaAggregate;
using Sempi5.Infrastructure;
using Sempi5.Services;
using Sempi5.Infrastructure.UserRepository;
using Sempi5.Infrastructure.PatientRepository;
using Sempi5.Infrastructure.AccoutToDeleteAggregate;
using Sempi5.Infrastructure.AccoutToDeleteRepository;
using Sempi5.Infrastructure.AppointmentAggregate;
using Sempi5.Infrastructure.AppointmentRepository;
using Sempi5.Infrastructure.ConfirmationLinkAggregate;
using Sempi5.Infrastructure.ConfirmationTokenAggregate;
using Sempi5.Infrastructure.ConfirmationTokenRepository;
using Sempi5.Infrastructure.LinkConfirmationRepository;
using Sempi5.Infrastructure.OperationRequestAggregate;
using Sempi5.Infrastructure.OperationRequestRepository;
using Sempi5.Infrastructure.OperationTypeAggregate;
using Sempi5.Infrastructure.OperationTypeRepository;
using Sempi5.Infrastructure.PatientAggregate;
using Sempi5.Infrastructure.PersonAggregate;
using Sempi5.Infrastructure.PersonRepository;
using Sempi5.Infrastructure.RequiredStaffAggregate;
using Sempi5.Infrastructure.RequiredStaffRepository;
using Sempi5.Infrastructure.SpecializationAggregate;
using Sempi5.Infrastructure.SpecializationRepository;
using Sempi5.Infrastructure.StaffAggregate;
using Sempi5.Infrastructure.SurgeryRoomAggregate;
using Sempi5.Infrastructure.SurgeryRoomRepository;
using Sempi5.Infrastructure.UserAggregate;
using Serilog;
using Serilog.Events;

namespace Sempi5
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            builder.Configuration.AddCommandLine(args);
            
            CreateLogginsMechanism(builder);
            CreateDataBase(builder);
            ConfigureMyServices(builder.Services);

            builder.Services.AddCors(options =>
            {
                options.AddPolicy("AllowSpecificOrigin",
                    policy =>
                    {
                        policy
                            .WithOrigins(builder.Configuration["FrontEnd:Url"]) // Make sure to use correct protocol (https/http)
                            .AllowAnyHeader()
                            .AllowAnyMethod()
                            .AllowCredentials();
                    });
            });

            builder.Services.AddAuthorization(options =>
            {
                options.AddPolicy("Staff", policy => policy.RequireRole("Doctor", "Nurse", "Admin", "Technician"));

                options.AddPolicy("Patient", policy => policy.RequireRole("Patient"));
            });

            builder.Services.AddAuthentication(options =>
                {
                    options.DefaultScheme = CookieAuthenticationDefaults.AuthenticationScheme;
                    options.DefaultChallengeScheme = CookieAuthenticationDefaults.AuthenticationScheme;
                })
                .AddCookie(CookieAuthenticationDefaults.AuthenticationScheme, options =>
                {
                    options.LoginPath = "/login/login";
                    options.AccessDeniedPath = "/error/access-denied";
                    options.ExpireTimeSpan = TimeSpan.FromHours(2);
                    options.SlidingExpiration = true;
                })
                .AddGoogle(options =>
                {
                    options.ClientId = builder.Configuration["Google:ClientId"];
                    options.ClientSecret = builder.Configuration["Google:Client_Secret"];
                    options.CallbackPath = "/signin-google";
                    options.SaveTokens = true;

                    options.Events.OnCreatingTicket = async context =>
                    {
                        var claimsIdentity = (ClaimsIdentity)context.Principal.Identity;
                        var claims = context.Principal.Identities.FirstOrDefault().Claims;
                        var email = claims.FirstOrDefault(x => x.Type == ClaimTypes.Email)?.Value;

                        var repo = context.HttpContext.RequestServices.GetRequiredService<IUserRepository>();
                        var user = await repo.GetByEmail(email);
                        if (user == null)
                        {
                            claimsIdentity.AddClaim(new Claim(ClaimTypes.Role, "Unregistered"));
                        }
                        else
                        {
                            if (user.IsVerified)
                            {
                                claimsIdentity.AddClaim(new Claim(ClaimTypes.Role, user.Role));
                            }
                            else
                            {
                                claimsIdentity.AddClaim(new Claim(ClaimTypes.Role, "Unverified"));
                            }
                        }
                    };
                    
                    options.Events.OnRemoteFailure = context =>
                    {
                        // Log failure details
                        Log.Error("Google OAuth Error: " + context.Failure?.Message);
                        context.Response.Redirect("/error/oauth-failure");
                        return Task.CompletedTask;
                    };
                });

            builder.Services.ConfigureApplicationCookie(options =>
            {
                options.Cookie.Domain = ".sarm.com";
                options.Cookie.SameSite = SameSiteMode.None;
                options.Cookie.SecurePolicy = CookieSecurePolicy.Always;
                options.ExpireTimeSpan = TimeSpan.FromHours(2);
                options.SlidingExpiration = true;
            });


            builder.Services.AddControllersWithViews();
            builder.Services.AddEndpointsApiExplorer();

            var app = builder.Build();

            if (app.Environment.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            app.UseHttpsRedirection();
            app.UseRouting();
            app.UseCors("AllowSpecificOrigin");
            app.UseAuthentication();
            app.UseAuthorization();
            app.MapControllers();

            try
            {
                SeedAllData(app.Services);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                Console.WriteLine("Error seeding data");
            }

            app.Run();
        }

        public static void SeedAllData(IServiceProvider services)
        {
            using (var scope = services.CreateScope())
            {
                var unitOfWork = scope.ServiceProvider.GetRequiredService<IUnitOfWork>();
                var userRepository = scope.ServiceProvider.GetRequiredService<IUserRepository>();
                var staffRepository = scope.ServiceProvider.GetRequiredService<IStaffRepository>();
                var patentRepository = scope.ServiceProvider.GetRequiredService<IPatientRepository>();
                var operationTypeRepository = scope.ServiceProvider.GetRequiredService<IOperationTypeRepository>();
                var appointmentRepository = scope.ServiceProvider.GetRequiredService<IAppointmentRepository>();
                var operationRequestRepository = scope.ServiceProvider.GetRequiredService<IOperationRequestRepository>();
                var surgeryRoomRepository = scope.ServiceProvider.GetRequiredService<ISurgeryRoomRepository>();
                var staffAgendaRepository = scope.ServiceProvider.GetRequiredService<IStaffAgendaRepository>();
                var roomAgendaRepository = scope.ServiceProvider.GetRequiredService<IRoomAgendaRepository>();

                new UsersBootstrap(userRepository).SeedAdminUser().Wait();
                unitOfWork.CommitAsync().Wait();

                var staffBootstrap = new StaffBootstrap(staffRepository);
                staffBootstrap.SeedActiveStaff().Wait();
                staffBootstrap.SeedStaffProfiles().Wait();
                unitOfWork.CommitAsync().Wait();

                var patientBootstrap = new PatientBootstrap(patentRepository);
                patientBootstrap.SeedActivePatients().Wait();
                unitOfWork.CommitAsync().Wait();

                var operationRequestBootstrap = new OperationRequestBootstrap(operationRequestRepository,appointmentRepository);
                operationRequestBootstrap.SeedOperationRequests().Wait();
                unitOfWork.CommitAsync().Wait();

                var operationTypeBootstrap = new OperationTypeBootstrap(operationTypeRepository);
                operationTypeBootstrap.SeedOperationTypes().Wait();
                unitOfWork.CommitAsync().Wait();

                var surgeryRoomBootstrap = new SurgeryRoomBootstrap(surgeryRoomRepository);
                surgeryRoomBootstrap.SeedSurgeryRooms().Wait();
                unitOfWork.CommitAsync().Wait();
                
                var timeTableBootstrap = new TimeTableBootstrap( staffAgendaRepository, roomAgendaRepository);
                timeTableBootstrap.SeeedTavbeTime().Wait();
                unitOfWork.CommitAsync().Wait();
            }
        }

        public static void CreateDataBase(WebApplicationBuilder builder)
        {
            string name = "Sempi5.Infrastructure.Databases." + builder.Configuration["DataBase:Type"];
            Type? dbType = Type.GetType(name);

            if (dbType == null)
            {
                Console.WriteLine("Database Type Invalid. Please check the configuration file!\nApplication will exit");
                Environment.Exit(2);
            }

            try
            {
                ((IDatabase)Activator.CreateInstance(dbType)).connectDB(builder);
            }
            catch (Exception)
            {
                Console.WriteLine("Database not found\nApplication will exit");
                Environment.Exit(3);
            }
        }


        private static void CreateLogginsMechanism(WebApplicationBuilder builder)
        {
            Log.Logger = new LoggerConfiguration()
                .MinimumLevel.Debug()
                .MinimumLevel.Override("Microsoft", LogEventLevel.Information)
                .Enrich.FromLogContext()
                .WriteTo.Console()
                .WriteTo.Logger(lc => lc
                    .Filter.ByIncludingOnly(e =>
                        e.Properties.ContainsKey("CustomLogLevel") &&
                        e.Properties["CustomLogLevel"].ToString() == "\"CustomLevel\"")
                    .WriteTo.File("Logs/log-.txt", rollingInterval: RollingInterval.Day))
                .CreateLogger();

            builder.Host.UseSerilog();
        }


        public static void ConfigureMyServices(IServiceCollection services)
        {
            services.AddTransient<IUnitOfWork, UnitOfWork>();

            services.AddTransient<ISpecializationRepository, SpecializationRepository>();
            services.AddTransient<IStaffRepository, StaffRepository>();
            services.AddTransient<IUserRepository, UserRepository>();
            services.AddTransient<IPatientRepository, PatientRepository>();
            services.AddTransient<IRequiredStaffRepository, RequiredStaffRepository>();
            services.AddTransient<IOperationTypeRepository, OperationTypeRepository>();
            services.AddTransient<IPersonRepository, PersonRepository>();
            services.AddTransient<IConfirmationTokenRepository, ConfirmationTokenRepository>();
            services.AddTransient<IOperationRequestRepository, OperationRequestRepository>();
            services.AddTransient<ISurgeryRoomRepository, SurgeryRoomRepository>();
            services.AddTransient<IAppointmentRepository, AppointmentRepository>();
            services.AddTransient<IAccountToDeleteRepository, AccountToDeleteRepository>();
            services.AddTransient<IConfirmationLinkRepository, ConfirmationLinkRepository>();
            services.AddTransient<IStaffAgendaRepository, StaffAgendaRepository>();
            services.AddTransient<IRoomAgendaRepository, RoomAgendaRepository>();

            services.AddTransient<StaffService>();
            services.AddTransient<LoginService>();
            services.AddTransient<EmailService>();
            services.AddTransient<PatientService>();
            services.AddTransient<TokenService>();
            services.AddTransient<OperationTypeService>();
            services.AddTransient<OperationRequestService>();
            services.AddTransient<SystemUserService>();
            services.AddTransient<SurgeryRoomService>();
            services.AddTransient<AgendaService>();

            services.AddSingleton(Log.Logger);
        }
    }
}