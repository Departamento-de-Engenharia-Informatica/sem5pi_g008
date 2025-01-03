import {Injectable} from '@angular/core';
import json from "../../appsettings.json"
import {HttpClient, HttpErrorResponse, HttpParams} from '@angular/common/http';
import {catchError, map, Observable, of, throwError} from 'rxjs';
import {Allergy} from '../../Domain/Allergy/Allergy';
import {AllergyMapper} from '../../DTOs/mappers/allergyMapper';
import {DisplayAllergyDTO} from '../../DTOs/displayDTOs/displayAllergyDTO';
import {BackendAllergyDTO} from '../../DTOs/backendDTOs/backendAllergyDTO';
import {CreateAllergyDTO} from '../../DTOs/createDTOs/createAllergyDTO';

@Injectable({
  providedIn: 'root',
})

export class AllergyService {

  private apiUrl = json.backendApi["2"].url + '/allergy';

  constructor(private http: HttpClient) {
  }

  addAllergy(allergyDTO: CreateAllergyDTO): Observable<string> {

    const allergy = AllergyMapper.createDtoToDomain(allergyDTO);
    const backendAllergyDTO = AllergyMapper.domainToBackendDto(allergy);

    return this.http.post<string>(this.apiUrl, backendAllergyDTO, {
      withCredentials: true,
    }).pipe(
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if (error.status >= 400 && error.status < 500) {
          errorMessage = error.error.message || 'There was an issue with your request. Please check the data you entered.';
        } else if (error.status >= 500 && error.status < 600) {
          errorMessage = error.error.message || 'An error occurred on the server. Please try again later.';
        } else if (error.error instanceof ErrorEvent) {
          errorMessage = error.error.message || 'A network error occurred. Please check your connection and try again.';
        }

        return throwError(errorMessage);
      })
    );
  }

  getAllAllergies(): Observable<{ allergies: Allergy[] }> {

    return this.http.get<{ allergies: Allergy[] }>(this.apiUrl, {
      withCredentials: true
    }).pipe(
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if (error.status >= 400 && error.status < 500) {
          errorMessage = error.error.message || 'There was an issue with your request. Please check the data you entered.';
        } else if (error.status >= 500 && error.status < 600) {
          errorMessage = error.error.message || 'An error occurred on the server. Please try again later.';
        } else if (error.error instanceof ErrorEvent) {
          errorMessage = error.error.message || 'A network error occurred. Please check your connection and try again.';
        }

        return throwError(errorMessage);
      })
    );
  }

  updateAllergyDesignation(id: string, newDesignation: string): Observable<void> {
    return this.http.patch<void>(`${this.apiUrl}/${id}/designation`, { designation: newDesignation }, { withCredentials: true }).pipe(
      catchError((error: HttpErrorResponse) => this.handleError(error))
    );
  }

  updateAllergyDescription(id: string, newDescription: string): Observable<void> {
    return this.http.patch<void>(`${this.apiUrl}/${id}/description`, { description: newDescription }, { withCredentials: true }).pipe(
      catchError((error: HttpErrorResponse) => this.handleError(error))
    );
  }

  updateAllergyEffects(id: string, newEffects: string[]): Observable<void> {
    return this.http.patch<void>(`${this.apiUrl}/${id}/effects`, { effects: newEffects }, { withCredentials: true }).pipe(
      catchError((error: HttpErrorResponse) => this.handleError(error))
    );
  }


  searchAllergies(allergy: string): Observable<{ allergies: DisplayAllergyDTO }> {
    const params = new HttpParams().set('allergy', allergy);

    return this.http
      .get<{ allergies: BackendAllergyDTO }>(this.apiUrl, {
        params: params,
        withCredentials: true,
      })
      .pipe(
        map((response) => {
          const allergyDomain = AllergyMapper.backendDtoToDomain(response.allergies);
          const displayAllergyDto = AllergyMapper.domainToDisplayDto(allergyDomain);

          return { allergies: displayAllergyDto };
        }),
        catchError((error: HttpErrorResponse) => {
          let errorMessage = 'An unknown error occurred.';

          if (error.status >= 400 && error.status < 500) {
            errorMessage =
              error.error.message ||
              'There was an issue with your request. Please check the data you entered.';
          } else if (error.status >= 500 && error.status < 600) {
            errorMessage =
              error.error.message ||
              'An error occurred on the server. Please try again later.';
          } else if (error.error instanceof ErrorEvent) {
            errorMessage =
              error.error.message ||
              'A network error occurred. Please check your connection and try again.';
          }

          return throwError(() => new Error(errorMessage));
        })
      );
  }

  private handleError(error: HttpErrorResponse): Observable<never> {
    let errorMessage = 'An unknown error occurred.';

    if (error.status >= 805 && error.status <= 809) {
      errorMessage = error.error?.message || 'Backend-specific error occurred.';
    } else if (error.status === 500) {
      errorMessage = error.error?.message || 'Internal server error.';
    }

    console.error('HTTP Error:', error);
    return throwError(() => new Error(errorMessage));
  }


}
