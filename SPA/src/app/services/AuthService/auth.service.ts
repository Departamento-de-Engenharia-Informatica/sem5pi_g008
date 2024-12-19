import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Router} from '@angular/router';
import {catchError, Observable, of} from 'rxjs';
import json from '../../appsettings.json';

@Injectable({
  providedIn: 'root'
})
export class AuthService {
  private apiUrl = json.backendApi["1"].url + '/Login';

  constructor(private http: HttpClient, private router: Router) {
  }

  login(): void {
    window.location.href = `${this.apiUrl}/login`;
  }

  logout(): void {
    window.location.href = `${this.apiUrl}/logout`;
  }

  getUserRole(): Observable<string | null> {
    return this.http.get(this.apiUrl + '/role', {
      withCredentials: true,
      responseType: 'text'
    }).pipe(
      catchError((error) => {
        console.error('Error retrieving user role', error);
        return of(null);
      })
    );
  }

  validateUserRole(...expectedRoles: string[]): void {
    this.getUserRole().subscribe(role => {
      if (!role || !expectedRoles.includes(role)) {
        this.router.navigate(['/']);
      }
    });
  }

}
