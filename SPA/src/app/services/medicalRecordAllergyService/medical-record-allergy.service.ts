import {Injectable} from '@angular/core';
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import json from '../../appsettings.json';
import {catchError, map, Observable, throwError} from 'rxjs';
import {MedicalRecordAllergyMapper} from "../../DTOs/mappers/medicalRecordAllergyMapper";
import {DisplayMedicalRecordAllergyDTO} from "../../DTOs/displayDTOs/displayMedicalRecordAllergyDTO";

@Injectable({
  providedIn: 'root'
})

export class MedicalRecordAllergyService {

  private apiUrl = json.backendApi['2'].url;

  constructor(private http: HttpClient) {
  }

  listAllergiesInMedicalRecord(medicalRecordId: string): Observable<DisplayMedicalRecordAllergyDTO[]> {
    const url = `${this.apiUrl}/medicalRecord/${medicalRecordId}/allergy`;

    return this.http.get<{ body: any[] }>(url, {withCredentials: true}).pipe(
        map((response: { body: any[] }) => {
          if (response && Array.isArray(response.body)) {
            return response.body.map((backendDto: any) => {
              const domainModel = MedicalRecordAllergyMapper.backendDtoToDomain(backendDto);
              return MedicalRecordAllergyMapper.domainToDisplayDto(domainModel);
            });
          } else {
            throw new TypeError('body is not an array or is missing');
          }
        }),
        catchError((error: HttpErrorResponse) => {
          let errorMessage = 'An unknown error occurred.';

          if (error.status === 404) {
            errorMessage = 'Medical record not found.';
          } else if (error.status === 500) {
            errorMessage = 'Internal server error.';
          }

          console.error('Error loading allergies:', error);

          return throwError(() => new Error(errorMessage));
        })
    );
  }
}
