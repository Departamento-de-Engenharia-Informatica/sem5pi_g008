import {Injectable} from '@angular/core';
import {HttpClient, HttpHeaders, HttpParams} from '@angular/common/http';
import {Observable} from 'rxjs';
import json from '../../appsettings.json';
import {SpecializationDTO} from '../../DTO/SpecializationDTO';

@Injectable({
  providedIn: 'root'
})
export class SpecializationService {
  private apiUrl = json.backendApi["1"].url + '/Specialization';

  constructor(private http: HttpClient) {
  }

  public listAllSpecializations(): Observable<SpecializationDTO[]> {

    return this.http.get<SpecializationDTO[]>(`${this.apiUrl}`, {withCredentials: true});
  }

  public listSpecializationByName(specializationName: string): Observable<SpecializationDTO> {


    return this.http.get<SpecializationDTO>(`${this.apiUrl}/${specializationName}`, {withCredentials: true});
  }

  public deleteSpecialization(specializationName: string): Observable<any> {

    return this.http.delete<SpecializationDTO>(`${this.apiUrl}/${specializationName}`, {withCredentials: true});
  }

  public createSpecialization(specializationDTO: SpecializationDTO): Observable<SpecializationDTO> {

    return this.http.post<SpecializationDTO>(`${this.apiUrl}`, specializationDTO, {withCredentials: true});
  }

  public updateSpecializationName(specializationId: number,specializationName: string): Observable<any> {

    const body = JSON.stringify(specializationName);

    return this.http.patch(`${this.apiUrl}/name/${specializationId}`, body, {
      withCredentials: true,
      headers: new HttpHeaders({
        'Content-Type': 'application/json',
      })
    });
  }

  public updateSpecializationDescription(specializationId: number,specializationDescription: string): Observable<any> {

      const body = JSON.stringify(specializationDescription);

      return this.http.patch(`${this.apiUrl}/description/${specializationId}`, body, {
        withCredentials: true,
        headers: new HttpHeaders({
          'Content-Type': 'application/json',
        })
      });
    }
}
