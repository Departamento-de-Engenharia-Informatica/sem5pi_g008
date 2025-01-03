import {Injectable} from '@angular/core';
import json from '../../appsettings.json';
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError, map, Observable, throwError} from 'rxjs';
import {MedicalRecordFreeText} from '../../Domain/MedicalRecordFreeText';
import {DisplayMedicalRecordFreeTextDTO} from '../../DTOs/displayDTOs/displayMedicalRecordFreeTextDTO';
import {MedicalRecordAllergyMapper} from '../../DTOs/mappers/medicalRecordAllergyMapper';
import {MedicalRecordFreeTextMapper} from '../../DTOs/mappers/medicalRecordFreeTextMapper';


@Injectable({
  providedIn:'root'
})

export class MedicalRecordFreeTextService{
  private apiUrl=json.backendApi["2"].url;

  constructor(private http: HttpClient) {
  }


  listMedicalRecordFreeTextByMedicalRecordId(medicalRecordId: string) : Observable<DisplayMedicalRecordFreeTextDTO[]>{
    const url = `${this.apiUrl}/medicalRecord/${medicalRecordId}/freeText`;

    return this.http.get<{ body: any[] }>(url, {withCredentials: true}).pipe(
      map((response: { body: any[] }) => {
        if (response && Array.isArray(response.body)) {
          return response.body.map((backendDto: any) => {
            const domainModel = MedicalRecordFreeTextMapper.backendDtoToDomain(backendDto);
            return MedicalRecordFreeTextMapper.domainToDisplayDto(domainModel);
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

        console.error('Error loading comments:', error);

        return throwError(() => new Error(errorMessage));
      })
    );
  }

  addMedicalRecordFreeText(medicalRecordFreeText: MedicalRecordFreeText ): Observable<{medicalRecordFreeText: MedicalRecordFreeText}>{
    const url = `${this.apiUrl}?recordNumberId=${medicalRecordFreeText.medicalRecordId}`;

    return this.http.post<{medicalRecordFreeText:MedicalRecordFreeText }>(url, {
      withCredentials:true
    }).pipe(
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if(error.status === 850) {
          errorMessage = error.error.message;
        }

        return throwError(errorMessage);
      })
    );



  }
}
