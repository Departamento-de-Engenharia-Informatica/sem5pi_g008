import {Injectable} from '@angular/core';
import json from "../../appsettings.json"
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError, Observable, of, throwError} from 'rxjs';
import {Allergy} from '../../Domain/Allergy';
import {AllergyMapper} from '../../DTOs/mappers/allergyMapper';


@Injectable({
  providedIn: 'root',
})

export class AllergyService {

  private apiUrl = json.backend2ApiUrl + '/allergy';

  constructor(private http: HttpClient) {
  }

  addAllergy(allergy: Allergy): Observable<string> {

    const allergyDTO = AllergyMapper.domainToBackendDto(allergy);

    return this.http.post<string>(this.apiUrl, allergyDTO, {
      withCredentials: true,
    }).pipe(
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if (error.status >= 400 && error.status < 500) {
          errorMessage = error.error.message || 'There was an issue with your request. Please check the data you entered.';
        }

        else if (error.status >= 500 && error.status < 600) {
          errorMessage = error.error.message || 'An error occurred on the server. Please try again later.';
        }

        else if (error.error instanceof ErrorEvent) {
          errorMessage = error.error.message ||'A network error occurred. Please check your connection and try again.';
        }

        return throwError(errorMessage);
      })
    );
  }

  getAllAllergies(): Observable<{ allergies: Allergy[] }> {

    return this.http.get<{allergies: Allergy[]}>(this.apiUrl, {
      withCredentials: true
    }).pipe(
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if (error.status >= 400 && error.status < 500) {
          errorMessage = error.error.message || 'There was an issue with your request. Please check the data you entered.';
        }

        else if (error.status >= 500 && error.status < 600) {
          errorMessage = error.error.message || 'An error occurred on the server. Please try again later.';
        }

        else if (error.error instanceof ErrorEvent) {
          errorMessage = error.error.message ||'A network error occurred. Please check your connection and try again.';
        }

        return throwError(errorMessage);
      })
    );
  }

}
